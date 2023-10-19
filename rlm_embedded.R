library(MASS)
library(nnet)
library(pROC)
library(PRROC)
library(caret)
library(readxl)
library(mlt)

train_data <- read_xlsx("C:/Users/ASUS/OneDrive/Train_adas.xlsx")
test_data <- read_xlsx("C:/Users/ASUS/OneDrive/Test_adas.xlsx")


#------------------------------------------------------------------------

#### METODOS EMBEDDED

X_train <- as.data.frame(train_data[, -which(names(train_data) == "Result")])
X_train <- X_train[,1:28]
y_train <- train_data$Result
X_test <- test_data[,1:28]
y_test <- test_data$Result

X_test$Proveniência <- as.factor(X_test$Proveniência)
X_test$Local_SU <- as.factor(X_test$Local_SU)
X_test$Género <- as.factor(X_test$Género)
X_test$Grupo_Diagn <- as.factor(X_test$Grupo_Diagn)
X_test$Alcoolico <- as.factor(X_test$Alcoolico)

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

categorical_cols <- which(sapply(X_train, is.factor))
X_train[, categorical_cols] <- lapply(X_train[, categorical_cols], factor)
X_train_processed <- model.matrix(~ . - 1, data = X_train)


trctrl <- trainControl(method = "cv",number=10)
# Ajustar os valores de alpha (1-LASSO, 0- Ridge, de 0 a 1- Elastic Net)

#hyperparameter_grid <- expand.grid(alpha = seq(0.1, 0.9, by = 0.1),  
#lambda = seq(0.1, 0.7, 0.05))

enetFit <- train(X_train_processed, y_train, 
                 method = "glmnet",
                 trControl=trctrl,
                 # Parametros alpha e lambda 
                 tuneGrid = data.frame(alpha=0.1,
                                       lambda=seq(0.1,0.7,0.05)))

coef_data$p_value <- 2 * (1 - pt(abs(coef_data$Coeficiente / coef_data$Std.Error), df = Inf))


# Extrai os coeficientes das variaveis selecionadas
coefficients <- coef(enetFit$finalModel, s = enetFit$bestTune$lambda)



best_alpha <- enetFit$bestTune$alpha   # 0.1

predictions <- predict(enetFit, newdata = X_test_processed, s = "lambda.min", type = "prob")
predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]


predicted_classes_factor <- factor(predicted_classes, levels = levels(y_test))

confusion <- table(predicted_classes_factor, y_test)
accuracy <- sum(diag(confusion)) / sum(confusion)  
precision <- diag(confusion) / colSums(confusion)
recall <- diag(confusion) / rowSums(confusion)
f1 <- 2 * (precision * recall) / (precision + recall)


class_labels <- c('Ausente', 'Hipoativo', 'Hiperativo')
auc_pr <- numeric(length(class_labels))
auc_roc <- numeric(length(class_labels))

for (class_label in class_labels) {
 
  binary_response <- ifelse(y_test == class_label, 1, 0)
  
  # Calcula AUC-PR
  pr_curve <- pr.curve(scores.class0 = as.numeric(predictions[, class_label]), weights.class0 = binary_response)
  auc_pr[class_label] <- pr_curve$auc.integral
  
  # Calcula AUC-ROC
  auc_roc[class_label] <- auc(roc(response = binary_response, predictor = as.numeric(predictions[, class_label])))
}

for (class_label in class_labels) {
  cat("Class:", class_label, "\n")
  cat("AUC-PR:", auc_pr[class_label], "\n")
  cat("AUC-ROC:", auc_roc[class_label], "\n")
}



# Calcula o OR como o exponencial dos coeficientes
OR <- exp(coefficients)


# Ordenar o dataframe por coeficiente em ordem decrescente
df_coeficientes <- df_coeficientes[order(-df_coeficientes$Coeficiente), ]

# Criar um gráfico de barras para visualizar a importância das variaveis
ggplot(df_coeficientes, aes(x = reorder(Variavel, Coeficiente), y = Coeficiente)) +
  geom_bar(stat = "identity", fill = "#98D8D8") +
  coord_flip() +
  labs(title = "Importância das variáveis para o subtipo Hipoativo",
       x = "Variáveis",
       y = "Coeficientes")




#### LASSO  ####

library(glmnet)
library(plotmo)
lasso <- glmnet(x=data.matrix(train_data[,-1]), y=train_data$Result, family = "multinomial", alpha=1,  type.multinomial = "grouped")
plot(lasso)
plot_glmnet(lasso, label = TRUE, nresponse = 3) # como coef variam com o lambda

coefficients <- coef(lasso, s = "lambda.min")

# variaveis selecionadas (== 0 sao excluidas)
rownames(coefficients$Hiperativo)[which(coefficients$Hiperativo != 0)]
rownames(coefficients$Hipoativo)[which(coefficients$Hipoativo != 0)]
rownames(coefficients$Ausente)[which(coefficients$Ausente != 0)]



# 10-fold cross validation
cvfit <- cv.glmnet(x=data.matrix(train_data[,-1]), y=train_data$Result, family="multinomial", type.multinomial='grouped',parallel = TRUE, alpha= 1, nfolds = 10)
#type.measure = "mse",
plot(cvfit)  # plot variable deviances vs. shrinkage parameter (lambda)

idealLambda <- cvfit$lambda.min



model <- glmnet(data.matrix(train_data[,-1]),
                train_data$Result, alpha = 1, lambda = idealLambda, family="multinomial")

# Coeficientes
coef(model)

x_test <- model.matrix(Result ~ ., data = test_data)
predicted_probs <- predict(model, newx = x_test, type = "response")


predicted_classes <- colnames(predicted_probs)[apply(predicted_probs, 1, which.max)]



#-----------------------------------------------------

# Avaliação da qualidade de ajuste do modelo

# Calcula a deviance do modelo ajustado
deviance_model <- deviance(model)

# Calcula a deviance do modelo nulo (modelo com apenas a interceção)
null_deviance <- deviance(update(model, . ~ 1))


# Calcula o pseudo-R^2 de McFadden
pseudo_r2_mcfadden <- 1 - (deviance_model / null_deviance)

# Calcula o pseudo-R^2 de Cox & Snell
pseudo_r2_cox_snell <- 1 - exp(-2/696 *(null_deviance - deviance_model))

# Calcula o pseudo-R^2 de Nagelkerke
pseudo_r2_nagelkerke <- (1 - exp((null_deviance / 696) - (deviance_model / 696))) / (1 - exp((null_deviance / 696)))


print(paste("Pseudo-R^2 de McFadden:", pseudo_r2_mcfadden))
print(paste("Pseudo-R^2 de Cox & Snell:", pseudo_r2_cox_snell))
print(paste("Pseudo-R^2 de Nagelkerke:", pseudo_r2_nagelkerke))


aic <- AIC(model)
bic <- BIC(model)


print(paste("AIC:", aic))
print(paste("BIC:", bic))
