#METRICAS SEM BALANCEAR OS DADOS

library(readxl)
library("writexl")
db <- read_excel("C:/Users/ASUS/OneDrive/final_database.xlsx")


data <- db[, ! names(db) %in% "RASS"]
library(caret)


set.seed(123)

# Divisao estratificada
split_indices <- createDataPartition(data$Result, p = 0.7, list = FALSE)

train_data <- data[split_indices, ]
test_data <- data[-split_indices, ]

table(train_data$Result)   # 0  1  2 =  238  56  10
table(test_data$Result)    # 0  1  2 =  104  18  8


# Normalizaçao dos dados
library(caret)
preprocessed_smote <- preProcess(train_data[,c(3,5,7:17)], method = c("range"))
scaledTrainData <- predict(preprocessed_smote, newdata = train_data)
scaledTestData <- predict(preprocessed_smote, newdata = test_data)



scaledTrainData[, c(1:2,4,6,18:29)] <- round(scaledTrainData[, c(1:2,4,6,18:29)])
scaledTestData[, c(1:2,4,6,18:29)] <- round(scaledTestData[, c(1:2,4,6,18:29)])



# Converter variaveis numericas em categoricas para iniciar a construçao do modelo
scaledTrainData$Proveniencia[scaledTrainData$Proveniencia == 0] <- "Casa"
scaledTestData$Proveniencia[scaledTestData$Proveniencia == 0] <- "Casa"
scaledTrainData$Proveniencia[scaledTrainData$Proveniencia == 1] <- "Lar"
scaledTestData$Proveniencia[scaledTestData$Proveniencia == 1] <- "Lar"
scaledTrainData$Proveniencia[scaledTrainData$Proveniencia == 2] <- "Intra-Hospitalar"
scaledTestData$Proveniencia[scaledTestData$Proveniencia == 2] <- "Intra-Hospitalar"
scaledTrainData$Proveniencia[scaledTrainData$Proveniencia == 3] <- "Inter-Hospitalar"
scaledTestData$Proveniencia[scaledTestData$Proveniencia == 3] <- "Inter-Hospitalar"
scaledTrainData$Proveniencia <- as.factor(scaledTrainData$Proveniencia)
scaledTestData$Proveniencia <- as.factor(scaledTestData$Proveniencia)

scaledTrainData$Local_SU[scaledTrainData$Local_SU == 0] <- "UDC1"
scaledTestData$Local_SU[scaledTestData$Local_SU == 0] <- "UDC1"
scaledTrainData$Local_SU[scaledTrainData$Local_SU == 1] <- "UDC2"
scaledTestData$Local_SU[scaledTestData$Local_SU == 1] <- "UDC2"
scaledTrainData$Local_SU[scaledTrainData$Local_SU == 2] <- "UCI"
scaledTestData$Local_SU[scaledTestData$Local_SU == 2] <- "UCI"
scaledTrainData$Local_SU[scaledTrainData$Local_SU == 3] <- "Ambulatorio"
scaledTestData$Local_SU[scaledTestData$Local_SU == 3] <- "Ambulatorio"
scaledTrainData$Local_SU<- as.factor(scaledTrainData$Local_SU)
scaledTestData$Local_SU<- as.factor(scaledTestData$Local_SU)

scaledTrainData$Genero[scaledTrainData$Genero == 0] <- "Masculino"
scaledTestData$Genero[scaledTestData$Genero == 0] <- "Masculino"
scaledTrainData$Genero[scaledTrainData$Genero == 1] <- "Feminino"
scaledTestData$Genero[scaledTestData$Genero == 1] <- "Feminino"
scaledTrainData$Genero <- as.factor(scaledTrainData$Genero)
scaledTestData$Genero <- as.factor(scaledTestData$Genero)


scaledTrainData$Grupo_Diagn[scaledTrainData$Grupo_Diagn == 0] <- "Neurologico"
scaledTestData$Grupo_Diagn[scaledTestData$Grupo_Diagn == 0] <- "Neurologico"
scaledTrainData$Grupo_Diagn[scaledTrainData$Grupo_Diagn == 1] <- "Cardiovascular"
scaledTestData$Grupo_Diagn[scaledTestData$Grupo_Diagn == 1] <- "Cardiovascular"
scaledTrainData$Grupo_Diagn[scaledTrainData$Grupo_Diagn == 2] <- "Gastrointestinal"
scaledTestData$Grupo_Diagn[scaledTestData$Grupo_Diagn == 2] <- "Gastrointestinal"
scaledTrainData$Grupo_Diagn[scaledTrainData$Grupo_Diagn == 3] <- "Respiratorio"
scaledTestData$Grupo_Diagn[scaledTestData$Grupo_Diagn == 3] <- "Respiratorio"
scaledTrainData$Grupo_Diagn[scaledTrainData$Grupo_Diagn == 4] <- "Geniturinario"
scaledTestData$Grupo_Diagn[scaledTestData$Grupo_Diagn == 4] <- "Geniturinario"
scaledTrainData$Grupo_Diagn[scaledTrainData$Grupo_Diagn == 5] <- "Musculo-Esqueletico"
scaledTestData$Grupo_Diagn[scaledTestData$Grupo_Diagn == 5] <- "Musculo-Esqueletico"
scaledTrainData$Grupo_Diagn[scaledTrainData$Grupo_Diagn == 6] <- "Toxi"
scaledTestData$Grupo_Diagn[scaledTestData$Grupo_Diagn == 6] <- "Toxi"
scaledTrainData$Grupo_Diagn[scaledTrainData$Grupo_Diagn == 7] <- "Outro"
scaledTestData$Grupo_Diagn[scaledTestData$Grupo_Diagn == 7] <- "Outro"
scaledTrainData$Grupo_Diagn[scaledTrainData$Grupo_Diagn == 8] <- "Hemato-oncologico"
scaledTestData$Grupo_Diagn[scaledTestData$Grupo_Diagn == 8] <- "Hemato-oncologico"
scaledTrainData$Grupo_Diagn <- as.factor(scaledTrainData$Grupo_Diagn)
scaledTestData$Grupo_Diagn <- as.factor(scaledTestData$Grupo_Diagn)


scaledTrainData$Alcoolico[scaledTrainData$Alcoolico == 0] <- "Nao"
scaledTestData$Alcoolico[scaledTestData$Alcoolico == 0] <- "Nao"
scaledTrainData$Alcoolico[scaledTrainData$Alcoolico == 1] <- "Sim"
scaledTestData$Alcoolico[scaledTestData$Alcoolico == 1] <- "Sim"
scaledTrainData$Alcoolico <- as.factor(scaledTrainData$Alcoolico)
scaledTestData$Alcoolico <- as.factor(scaledTestData$Alcoolico)

scaledTrainData$Data_Obito[scaledTrainData$Data_Obito == 0] <- "Vivo"
scaledTestData$Data_Obito[scaledTestData$Data_Obito == 0] <- "Vivo"
scaledTrainData$Data_Obito[scaledTrainData$Data_Obito == 1] <- "Morto"
scaledTestData$Data_Obito[scaledTestData$Data_Obito == 1] <- "Morto"
scaledTrainData$Data_Obito <- as.factor(scaledTrainData$Data_Obito)
scaledTestData$Data_Obito <- as.factor(scaledTestData$Data_Obito)

scaledTrainData$Result[scaledTrainData$Result == 0] <- "Ausente"
scaledTestData$Result[scaledTestData$Result == 0] <- "Ausente"
scaledTrainData$Result[scaledTrainData$Result == 1] <- "Hipoativo"
scaledTestData$Result[scaledTestData$Result == 1] <- "Hipoativo"
scaledTrainData$Result[scaledTrainData$Result == 2] <- "Hiperativo"
scaledTestData$Result[scaledTestData$Result == 2] <- "Hiperativo"
scaledTrainData$Result <- as.factor(scaledTrainData$Result)
scaledTestData$Result <- as.factor(scaledTestData$Result)


encode_variables <- function(data, variables) {
  for (variable in variables) {
    data[[variable]] <- ifelse(data[[variable]] == 0, 'ausente', 'presente')
  }
  return(data)
}


# Categorizar os grupos farmacologicos em presente e ausente
variables_encode <- c('Corticosteroides', 'Cardiotonicos', 'Outros_Med', 'Antihipertensivos', 'Analgesicos', 'Antidislipidemicos', 'Ansioliticos', 'Antidepressivos', 'Antipsicoticos', 'Anticoagulantes')
scaledTrainData <- encode_variables(scaledTrainData, variables_encode)
scaledTestData <- encode_variables(scaledTestData, variables_encode)


table(scaledTrainData$Result)
barplot(table(scaledTrainData$Result))

table(scaledTestData$Result)
barplot(table(scaledTestData$Result))


#---------------------------------------

library(MASS)
library(nnet)
library(pROC)
library(caret)

train_data <- scaledTrainData
test_data <- scaledTestData

train_data <- train_data[,-29]
test_data <- test_data[,-29]


#  Treina o modelo de regressao logistica multinomial
model <- multinom(Result ~ ., data = train_data)


# Calcula AIC
aic <- AIC(model)


# Gera previsoes
predictions <- predict(model, newdata = test_data, type = "class")


confusion <- table(predictions, test_data$Result)

# Calcula acuracia
accuracy <- sum(diag(confusion)) / sum(confusion)

# Calcula sensibilidade
recall <- diag(confusion) / rowSums(confusion)

# Calcula precisao
precision <- diag(confusion) / colSums(confusion)

# Calcula F1 
f1 <- 2 * (precision * recall) / (precision + recall)




test_data$Result <- factor(test_data$Result, levels = c("Ausente", "Hipoativo", "Hiperativo"))
predictions <- predict(model, newdata = test_data, type = "probs")

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



cat("Accuracy:", accuracy, "\n")
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1, "\n")
cat("AUC-PR:", auc_pr, "\n")
cat("AUC-ROC:", auc_roc, "\n")


