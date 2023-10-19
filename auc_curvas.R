library(readxl)
library(ggplot2)
library(gridExtra)
library(caret)
library(nnet)
library(MASS)
library(PRROC)
library(pROC)

train_data <- read_excel("C:/Users/ASUS/OneDrive/Train_adas.xlsx")
test_data <- read_excel("C:/Users/ASUS/OneDrive/Test_adas.xlsx")


train_data$Result <- as.factor(train_data$Result)
test_data$Result <- as.factor(test_data$Result)
train_data <- train_data[,-29]
test_data <- test_data[,-29]

# proporção dos individuos no dataset de teste para cada classe 
mean(train_data$Result == 'Ausente')  #0.342
mean(train_data$Result == 'Hipoativo') #0.320
mean(train_data$Result == 'Hiperativo') #0.338

# plots exploratorios 
p1 <- ggplot(train_data, aes(x=Result, y=Idade, fill=Result)) + geom_boxplot()  
p2 <- ggplot(train_data, aes(x=Result, y=Interna_Dias, fill=Result)) + geom_boxplot()  
p3 <- ggplot(train_data, aes(x=Result, y=SIRS, fill=Result)) + geom_boxplot()

grid.arrange(p1, p2, p3, ncol=3)

mod <- multinom(Result ~., train_data)
summary(mod)
predict(mod, test_data, type="probs")
training_pred <- predict(mod, test_data, type="class")

cf <- summary(mod)$coefficients
accuracy <- mean(training_pred == train_data$Result)
confusionMatrix(training_pred, train_data$Result)



#########################
library(tidyverse) 
library(dplyr)
library(tidymodels)



initial_model <- multinom(Result ~ ., data = train_data)
selected_model <- stepAIC(initial_model, direction = "backward")

summary(selected_model)

predictions <- predict(selected_model, newdata = test_data)



# Calcula as probabilidades previstas
predicted_probabilities <- predict(selected_model, newdata = test_data, type = "probs")

# Extrai rótulos reais para as categorias
true_labels <- test_data$Result

# Cria um vetor para armazenar as curvas AUC-ROC de cada classe
roc_list <- list()

# Calcula a curva AUC-ROC para cada categoria usando a estratégia OvA
for (class_label in unique(true_labels)) {
  # Cria um vetor binário para a classe atual
  true_labels_binary <- ifelse(true_labels == class_label, 1, 0)
  
  # Calcula a curva AUC-ROC para a classe atual
  roc_obj <- roc(true_labels_binary, predicted_probabilities[, class_label])
  roc_list[[class_label]] <- roc_obj
}


# Cria um único gráfico de AUC-ROC para todas as classes
plot(roc_list[[1]], main = "Curvas AUC-ROC para Todas as Classes", col = "black", lwd = 2)

# Adiciona as curvas AUC-ROC para as outras classes
for (i in 2:length(roc_list)) {
  lines(roc_list[[i]], col = i, lwd = 2)
}

# Adiciona uma legenda
legend("bottomright", legend = names(roc_list), col = 1:length(roc_list), lwd = 2)



.pred_Hiperativo <- predicted_probabilities[,2]
.pred_Hipoativo <- predicted_probabilities[,3]

roc_auc(predictions, truth = Result,.pred_Hipoativo, .pred_Hiperativo)

roc_curve(preds, truth = Result, .pred_Hipoativo, .pred_Hiperativo) |> 
  ggplot(aes(x = 1 - especificidade, y = sensibilidade, color = .level)) +
  geom_line(size = 1, alpha = 0.7) +
  geom_abline(slope = 1, linetype = "dotted") +
  coord_fixed() +
  labs(color = NULL) +
  theme_light()





