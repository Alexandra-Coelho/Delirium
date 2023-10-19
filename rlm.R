library(MASS)
library(nnet)
library(pROC)
library(PRROC)
library(caret)
library(readxl)
library(mlt)

train_data <- read_xlsx("C:/Users/ASUS/OneDrive/Train_adas.xlsx")
test_data <- read_xlsx("C:/Users/ASUS/OneDrive/Test_adas.xlsx")


colnames(train_data)[which(names(train_data) == "Data_óbito")] <- "Obito"
colnames(test_data)[which(names(test_data) == "Data_óbito")] <- "Obito"
train_data <- train_data[,-29]
test_data <- test_data[,-29]

dataset <- rbind(train_data, test_data)

test_without <- test_data[, -which(names(test_data) == "Result")]


train_data$Result <- as.factor(train_data$Result)
test_data$Result <- as.factor(test_data$Result)

train_data$Result  <- relevel(train_data$Result , ref = "Ausente")
test_data$Result  <- relevel(test_data$Result , ref = "Ausente")


# METODOS PARA SELECIONAR VARIAVEIS PARA IMPLEMENTAR RLM 


####### RLM SEM FEATURE SELECTION USING ADASYN ###########


# Treino do modelo de regressao logistica multinomial
model <- multinom(Result ~ ., data = train_data, maxit=500, trace=T)
summary(model)

mostImportantVariables <- varImp(model)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(head(mostImportantVariables)) 


# Gera previsoes usando o modelo
predictions <- predict(model, newdata = test_data, type = "class")

confusion_matrix <- confusionMatrix(predictions, test_data$Result)

confusion <- table(predictions, test_data$Result)

# Calcula AIC
aic <- AIC(model)

# Calcula acuracia
accuracy <- sum(diag(confusion)) / sum(confusion)

# Calcula sensibilidade
recall <- diag(confusion) / rowSums(confusion)

# Calcula precisao
precision <- diag(confusion) / colSums(confusion)

# Calcula F1 
f1 <- 2 * (precision * recall) / (precision + recall)


predictions <- predict(model, newdata = test_data, type = "probs")

# Converte a variavel resposta em fator
test_data$Result <- factor(test_data$Result, levels = c("Ausente", "Hipoativo", "Hiperativo"))
auc_pr <- numeric(3)
auc_roc <- numeric(3)

for (i in 1:3) {
  scores <- predictions[, i]
  target <- as.numeric(test_data$Result == levels(test_data$Result)[i])
  
  # Calcula AUC-PR
  pr_curve <- pr.curve(scores, target, curve = TRUE)
  auc_pr[i] <- pr_curve$auc.integral
  
  # Calcula AUC-ROC
  roc_curve <- roc(target, scores)
  auc_roc[i] <- auc(roc_curve)
}

# Print das metricas
cat("Accuracy:", accuracy, "\n")
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1, "\n")
cat("AUC-PR:", auc_pr, "\n")
cat("AUC-ROC:", auc_roc, "\n")


library(gt)

# Cria um data frame para guardar as metricas
metrics <- data.frame(Class = c("Ausente", "Hipoativo", "Hiperativo"),
                      Accuracy = accuracy,
                      Recall = recall,
                      Precision = precision,
                      F1 = f1,
                      AUC_PR = auc_pr,
                      AUC_ROC = auc_roc)

table_output <- gt(metrics, rowname_col = "Class") %>%

  fmt_number(
    columns = vars(Accuracy:AUC_ROC),
    decimals = 2
  )

# Print a tabela
print(table_output)



                              #### STEPWISE METHOD = FORWARD ####


initial_model <- multinom(Result ~ ., data = train_data)
summary(initial_model)
final_model <- step(initial_model, direction = "forward")
summary(final_model)  #28 variables

coef(final_model)

mostImportantVariables <- varImp(final_model)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(head(mostImportantVariables))  #glicose,idade, PCR, pH, pO2, local_SU uci


predictions <- predict(final_model, newdata = test_data, type = "class")
test_data$Result <- factor(test_data$Result, levels = levels(predictions))
confusion_matrix <- confusionMatrix(predictions, test_data$Result)
#classification_report <- caret::confusionMatrix(predictions, test_classes)$byClass


selected_vars <- colnames(attr(final_model$terms, "factors"))
print(selected_vars)



confusion <- table(predictions, test_data$Result)


aic <- AIC(step.model)


accuracy <- sum(diag(confusion)) / sum(confusion)


recall <- diag(confusion) / rowSums(confusion)


precision <- diag(confusion) / colSums(confusion)


f1 <- 2 * (precision * recall) / (precision + recall)


predictions <- predict(final_model, newdata = test_data, type = "probs")
class_labels <- c('Ausente', 'Hipoativo', 'Hiperativo')
auc_pr <- numeric(length(class_labels))
auc_roc <- numeric(length(class_labels))

for (class_label in class_labels) {

  binary_response <- ifelse(y_test == class_label, 1, 0)
  
  pr_curve <- pr.curve(scores.class0 = as.numeric(predictions[, class_label]), weights.class0 = binary_response)
  auc_pr[class_label] <- pr_curve$auc.integral
  
  auc_roc[class_label] <- auc(roc(response = binary_response, predictor = as.numeric(predictions[, class_label])))
}

for (class_label in class_labels) {
  cat("Class:", class_label, "\n")
  cat("AUC-PR:", auc_pr[class_label], "\n")
  cat("AUC-ROC:", auc_roc[class_label], "\n")
}




################### BACKWARD #########################

initial_model <- multinom(Result ~ ., data = train_data)
selected_model <- stepAIC(initial_model, direction = "backward")


summary(selected_model)

predictions <- predict(selected_model, newdata = test_data)

# Avaliação do desempenho do modelo

confusion_matrix <- table(predictions, test_data$Result)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
f1_score <- 2 * precision * recall / (precision + recall)

predictions <- predict(selected_model, newdata = test_data, type = "probs")
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


print(confusion_matrix)
print(accuracy)
print(precision)
print(recall)
print(f1_score)




                           #### STEPWISE METHOD = BIDIRECIONAL ####


model <- multinom(Result ~ ., data = train_data)

# Implementa seleçao sequencial bidirecional usando o AIC como criterio

selected_model <- step(model, direction = "both", k = log(nrow(train_data)), trace = FALSE) #scope = formula(model),
summary(selected_model)

# Gera previsoes com o modelo selecionado
predictions <- predict(selected_model, newdata = test_data)


confusion_matrix <- table(predictions, test_data$Result)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
f1_score <- 2 * precision * recall / (precision + recall)

test_data$Result <- factor(test_data$Result, levels = c("Ausente", "Hipoativo", "Hiperativo"))
predictions <- predict(selected_model, newdata = test_data, type = "probs")

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


print(confusion_matrix)
print(accuracy)
print(precision)
print(recall)
print(f1_score)





##### RFE #####

library(randomForest)

# Define a funçao RFE control 
ctrl <- rfeControl(
  functions = rfFuncs,
  method = "cv",
  number = 10,
  verbose = TRUE
)

# Cria uma funçao para RFE com a regressao logistica multinomial
rfe_multinom <- function(data, indices, outcome, ...) {
  
  # Subconjunto dos dados utilizando os indices dados
  train_data_subset <- data[indices, ]
  
  # Treina o modelo de regressao logistica multinomial
  modelo <- multinom(outcome ~ ., data = train_data_subset)
  
  # Gera previsoes
  predictions <- predict(modelo, newdata = test_data)
  resultado$pred <- predictions
  
  # Calcula acuracia
  acc <- mean(predictions == test_data$Result)
  
  return(acc)
}

# Executa RFE
resultado <- rfe(train_data[, -ncol(train_data)], train_data$Result, sizes = tamanhos_recursos, 
                 rfeControl = ctrl, functions = rfFuncs)


print(resultado)
#The top 5 variables (out of 23): Idade, PCR, Creatinina, Interna_Dias, Proveniência

# Variáveis selecionadas
selected_variables <- rownames(resultado$optVariables)

print(selected_variables)


#------------------------------------------------------------------------


# cv-forward 

train_data <- as.data.frame(train_data)
colnames(train_data)[4] <- "Genero"
colnames(train_data)[19] <- "Antidislipidemicos"
colnames(train_data)[20] <- "Ansioliticos"
colnames(train_data)[21] <- "Analgesicos"
train_data$Genero <- as.factor(train_data$Genero)

# Cria um objeto task para a seleçao de varaiveis
task_train <- makeClassifTask(data = train_data, target = "Result")

# Define o metodo forward par a seleçao de variaveis 
fctrl <- makeFeatSelControlSequential(method = "sfs")

# Cria objeto resamppling stratified k-fold 
resampling_stratified <- makeResampleDesc("CV", iters = 10, stratify = TRUE)

# Implementa a seleçao forward no treino
selected_features <- selectFeatures(task_train, learner = "classif.multinom", control = fctrl, resampling = resampling_stratified)


# Extrai o nome das varaiveis do objeto selected_features 
selected_feature_names <- selected_features$x

# Adiciona "Result" as variaveis selecionadas
selected_feature_names <- c(selected_feature_names, "Result")  

# Subconjunto de treino e teste com as variaveis selecionadas 
train_data_subset <- train_data[, selected_feature_names]
selected_feature_names <- gsub("Genero", "Género", selected_feature_names)
colnames(train_data_subset)[colnames(train_data_subset) == "Genero"] <- "Género"
test_data_subset <- test_data[, selected_feature_names]

model <- multinom(Result ~ ., data = train_data_subset)

# Gera previsoes
predictions <- predict(model, newdata = test_data)
predictions <- as.factor(predictions)
test_data_subset$Result <- as.factor(test_data_subset$Result)

levels(predictions) <- levels(test_data_subset$Result)
confusion <- confusionMatrix(predictions, test_data_subset$Result)


confusion <- table(predictions, test_data$Result)


accuracy <- sum(diag(confusion)) / sum(confusion)  #0.62


recall <- diag(confusion) / rowSums(confusion)


precision <- diag(confusion) / colSums(confusion)


f1 <- 2 * (precision * recall) / (precision + recall)



predictions <- predict(model, newdata = test_data, type = "probs")


test_data$Result <- factor(test_data$Result, levels = c("Ausente", "Hipoativo", "Hiperativo"))

auc_pr <- numeric(3)
auc_roc <- numeric(3)

for (i in 1:3) {
  scores <- predictions[, i]
  target <- as.numeric(test_data$Result == levels(test_data$Result)[i])
  
  # Calcula AUC-PR
  pr_curve <- pr.curve(scores, target, curve = TRUE)
  auc_pr[i] <- pr_curve$auc.integral
  
  # Calcula AUC-ROC
  roc_curve <- roc(target, scores)
  auc_roc[i] <- auc(roc_curve)
}


cat("Accuracy:", accuracy, "\n")
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1, "\n")
cat("AUC-PR:", auc_pr, "\n")
cat("AUC-ROC:", auc_roc, "\n")

