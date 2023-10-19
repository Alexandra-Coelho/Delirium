library(readxl)
library(dplyr)
library("writexl")
db <- read_excel("C:/Users/ASUS/OneDrive/final_database.xlsx")

data <- db[, ! names(db) %in% "RASS"]
library(caret)


db
# Definir seed para reproduzir resultados
set.seed(123)

# Executa a divisao estratificada
split_indices <- createDataPartition(data$Result, p = 0.7, list = FALSE)

train_data <- data[split_indices, ]
test_data <- data[-split_indices, ]

table(train_data$Result)   # 0  1  2 =  238  56  10
table(test_data$Result)    # 0  1  2 =  104  18  8


library(smotefamily)


#ADASYN
sub <- train_data[train_data$Result %in% c(0,1), ]    #dataset so com class = 0 e 1 (238  56)
adas_1 <- ADAS(sub[, -30], sub$Result, K = 5)
adas_1 <- adas_1$data  # 0  1  =  238  223
sub2 <- train_data[train_data$Result %in% c(0,2), ]   #dataset so com class = 0 e 2 (238  10)
adas_2 <- ADAS(sub2[, -30], sub2$Result, K = 5)
adas_2 <- adas_2$data  # 0  1  =  238  235

merged_adas <- full_join(adas_1, adas_2)
colnames(merged_adas)[30] <- "Result"
table(merged_adas$Result)       # 0  1  2 = 238  223  235
#total=696 obs


# Normalizar os dados
library(caret)
preprocessed_smote <- preProcess(merged_adas[,c(3,5,7:17)], method = c("range"))
scaledTrainData <- predict(preprocessed_smote, newdata = merged_adas)
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


write_xlsx(scaledTrainData, "C:/Users/ASUS/OneDrive/Train_adas.xlsx")
write_xlsx(scaledTestData, "C:/Users/ASUS/OneDrive/Test_adas.xlsx")



####### RLM SEM FEATURE SELECTION USING ADASYN ###########

library(MASS)
library(nnet)

train_data <- scaledTrainData
test_data <- scaledTestData

train_data <- train_data[,-29]
test_data <- test_data[,-29]

# Treina o modelo de regressao logistica multinomial com os dados de treino
model <- multinom(Result ~ ., data = train_data)

# Calcula AIC
aic <- AIC(model)


# Calcula as previsoes para os dados de teste
predictions <- predict(model, newdata = test_data, type = "class")

# Cria matriz de confusao
confusion <- table(predictions, test_data$Result)

# Calcula acuracia
accuracy <- sum(diag(confusion)) / sum(confusion)

# Calcula sensibilidade
recall <- diag(confusion) / rowSums(confusion)

# Calcula precisao
precision <- diag(confusion) / colSums(confusion)

# Calcula F1 
f1 <- 2 * (precision * recall) / (precision + recall)



library(mlt)
# Converte a variavel resposta em factor com os niveis apropriados
test_data$Result <- factor(test_data$Result, levels = c("Ausente", "Hipoativo", "Hiperativo"))
predictions <- predict(model, newdata = test_data, type = "probs")


class_labels <- c('Ausente', 'Hipoativo', 'Hiperativo')
auc_pr <- numeric(length(class_labels))
auc_roc <- numeric(length(class_labels))

library(pROC)
library(PRROC)

for (class_label in class_labels) {
  # Cria uma resposta binaria para a classe atual
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

# Print das metricas
cat("Acuracia:", accuracy, "\n")
cat("Sensibilidade:", recall, "\n")
cat("Precisao:", precision, "\n")
cat("F1:", f1, "\n")
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

# Print da tabela
print(table_output)

