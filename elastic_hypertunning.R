library(readxl)
library(caret)
library(nnet)
library(glmnet)
train_data <- read_excel("C:/Users/ASUS/OneDrive/Train_adas.xlsx")
test_data <- read_excel("C:/Users/ASUS/OneDrive/Test_adas.xlsx")

# Ajuste de hiperparametros no nnet

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,            # Numero folds
                     repeats = 3,            # Numero de repetiçoes
                     classProbs = TRUE,      # Gera probabilidades para cada classe
                     savePredictions="all",
                     summaryFunction = multiClassSummary,  # Funçao sumario para dados multiclasse
                     selectionFunction = "oneSE"  # Seleçao do modelo com base no one standard error
)

model <- train(Result~., data=train_data,
               method = "nnet",
               trControl = ctrl,
               tuneGrid = expand.grid(.size = 1:10, .decay = c(0, 0.1, 0.01)),  # Varying the size and decay of the hidden layer
               trace = FALSE)

# size=7 e decay=0.01

print(model)

# Gera as previsões 

predictions <- predict(model, newdata = test_data)

confusion <- table(predictions, test_data$Result)

accuracy <- sum(diag(confusion)) / sum(confusion)

recall <- diag(confusion) / rowSums(confusion)

precision <- diag(confusion) / colSums(confusion)

f1 <- 2 * (precision * recall) / (precision + recall)


# Ajuste de hiperparametros para pacote glmnet

################## elastic net #################
library(glmnet)

# Converte a variavel resposta num fator
y_train <- factor(y_train)

# Converte as variaveis categoricas em fatores
categorical_cols <- which(sapply(X_train, is.factor))
X_train[, categorical_cols] <- lapply(X_train[, categorical_cols], factor)

# Cria variaveis dummy usando model.matrix
X_train_processed <- model.matrix(~ . - 1, data = X_train)

# Normaliza as variaveis numericas 
numeric_cols <- sapply(X_train_processed, is.numeric)
X_train_processed[, numeric_cols] <- scale(X_train_processed[, numeric_cols])

# Define o controlo do treino
trctrl <- trainControl(method = "cv", number = 10)

enetFit <- train(X_train_processed, y_train, 
                 method = "glmnet",
                 trControl=trctrl,
                 # parametros alpha e lambda a experimentar
                 tuneGrid = data.frame(alpha=0.1,
                                       lambda=seq(0.1,0.9,0.1)))


# Gera as previsoes com base no valor do lambda otimo obtido
predictions <- predict(enetFit, newdata = X_train_processed, s = "lambda.min", type = "class")

# Avaliaçao do modelo
confusion_matrix <- confusionMatrix(predictions, y_train)
accuracy <- confusion_matrix$overall["Accuracy"]
precision <- confusion_matrix$byClass["Precision"]
recall <- confusion_matrix$byClass["Recall"]
f1 <- confusion_matrix$byClass["F1"]


cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1, "\n")



# Index do lambda.min que corresponde ao lambda otimo
lambda_index <- which(enetFit$finalModel$lambda == enetFit$bestTune$lambda)

# Coeficientes do valor de lambda selecionado
coefficients <- enetFit$finalModel$beta[, lambda_index]

# Identifica as variaveis selecionadas (coeficientes !=0)
selected_variables <- colnames(coefficients)[coefficients != 0]

cat("Selected variables:", selected_variables, "\n")


# Treino o modelo glmnet usando o valor de lambda selecionado
selected_lambda <- enetFit$bestTune$lambda
glmnet_model <- cv.glmnet(as.matrix(X_train_processed), y_train, alpha = 0.5, lambda = selected_lambda)

# Coeficientes do modelo glmnet treinado
coefficients <- coef(glmnet_model, s = selected_lambda)


##################### RIDGE, MUDAR VALOR DE ALPHA PARA LASSO OU ELASTIC NET #################

X_train <- as.data.frame(train_data[, -which(names(train_data) == "Result")])
y_train <- train_data$Result
X_test <- test_data[,1:29]
y_test <- test_data$Result

X_test$Proveniência <- as.factor(X_test$Proveniência)
X_test$Local_SU <- as.factor(X_test$Local_SU)
X_test$Género <- as.factor(X_test$Género)
X_test$Grupo_Diagn <- as.factor(X_test$Grupo_Diagn)
X_test$Alcoolico <- as.factor(X_test$Alcoolico)
X_test$Data_?bito <- as.factor(X_test$Data_?bito)

categorical_cols <- which(sapply(X_test, is.factor))
X_test[, categorical_cols] <- lapply(X_test[, categorical_cols], factor)
X_test_processed <- model.matrix(~ . - 1, data = X_test)

y_train <- (factor(y_train))
y_test <- factor(y_test)


X_train$Proveniência <- as.factor(X_train$Proveniência)
X_train$Local_SU <- as.factor(X_train$Local_SU)
X_train$Género <- as.factor(X_train$Género)
X_train$Grupo_Diagn <- as.factor(X_train$Grupo_Diagn)
X_train$Alcoolico <- as.factor(X_train$Alcoolico)
X_train$Data_?bito <- as.factor(X_train$Data_?bito)

categorical_cols <- which(sapply(X_train, is.factor))
X_train[, categorical_cols] <- lapply(X_train[, categorical_cols], factor)
X_train_processed <- model.matrix(~ . - 1, data = X_train)

# Define a grid de hiperparametros a procurar
hyperparameter_grid <- expand.grid(alpha = seq(0, 1, by = 0.1),  # Ajustar o valor de alpha
                                   lambda = seq(0.1, 0.7, 0.05))

# Cria as definiçoes do train control para cross-validation
trctrl <- trainControl(method = "cv", number = 10)

# Treino oe modelo com diferentes valores de alpha e lambda 
enetFit <- train(X_train_processed, y_train, 
                 method = "glmnet",
                 trControl = trctrl,
                 tuneGrid = hyperparameter_grid)

# Print os resultados do modelo com melhor desempenho
print(enetFit)

# Acede aos melhores hiperparametros do modelo treinado
best_alpha <- enetFit$bestTune$alpha

# Print o melhor valor de alpha 
cat("Best Alpha:", best_alpha, "\n")

# Extrai os coeficientes das variaveis selecionadas
coefficients <- coef(enetFit$finalModel, s = enetFit$bestTune$lambda)

# Print os coeficientes
print(coefficients)

predictions <- predict(enetFit, newdata = X_test_processed, s = "lambda.min", type = "prob")
predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]

# Converte classes previstas e y_test para fatores com o mesmo nivel
predicted_classes_factor <- factor(predicted_classes, levels = levels(y_test))


confusion <- table(predicted_classes_factor, y_test)
accuracy <- sum(diag(confusion)) / sum(confusion)  
precision <- diag(confusion) / colSums(confusion)
recall <- diag(confusion) / rowSums(confusion)
f1 <- 2 * (precision * recall) / (precision + recall)
