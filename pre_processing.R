library(readxl)
library(summarytools)
library("writexl")
library("dplyr")
library("Amelia")
library("ggplot2")
library(caret)
#install.packages("summarytools")
#install.packages("Amelia")

# Carregamento e visualizaçãoo do dataset
dados <- read_xlsx("C:/Users/ASUS/OneDrive/DadosLimposComNrProcesso.xlsx")
head(dados)
tail(dados)

# Informação preliminar do dataset
dim(dados)
features <- colnames(dados)
features

# vivo = 0, morto = 1
dados$Data_Obito[dados$Data_Obito == "#NULL!"] <- 0
dados$Data_Obito[dados$Data_Obito != "0"] <- 1

dados$Data <- as.numeric(as.character(dados$Data)) 
dados$Hora <- as.numeric(as.character(dados$Hora)) 
dados$Data_Obito <- as.numeric(as.character(dados$Data_?bito)) 
table(dados$Data_Obito)

data <- dados[0:500,]
new_data <- data[apply(data, 1,function(i) !all(is.na(i))), ]
new_data <- data.frame(new_data)

write_xlsx(newdata, "C:/Users/ASUS/OneDrive/database.xlsx")

dim(new_data)
str(new_data)
summary(new_data)

table(sapply(new_data, class))
names(new_data[, sapply(new_data, class) == 'character'])  # nomes variaveis type=char

# converter em numeric
new_data[, c("HCO3...30", "Freq_Resp", "Freq_Card", "Temp_Corporal", "Cont_Leucocitos", "AC0", "AC1", "AC2_3", "AC_mais_q_3", "AC_1_todos", "AC_2_3_todos", "RASS_3")] <- lapply(new_data[, c("HCO3...30", "Freq_Resp", "Freq_Card", "Temp_Corporal", "Cont_Leucocitos", "AC0", "AC1", "AC2_3", "AC_mais_q_3", "AC_1_todos", "AC_2_3_todos", "RASS_3")], as.numeric)


# Visualização gráfica para cada variável
barplot(table(new_data$Proveniencia), main='Local de Proveniencia')
barplot(table(new_data$Local_SU), main='Local de SU')
hist((new_data$Idade), main='Idade', xlab = 'anos')
barplot(table(new_data$Genero), main='Genero')
barplot(table(new_data$SIRS...19), main='Critérios SIRS')
barplot(table(new_data$RASS...38), main='Critérios RASS')         

table(new_data$resulta_hipo_hiper)
barplot(table(new_data$resulta_hipo_hiper), main='Resultado diagnostico')


# Verificaçao da hipótese de existir dados em falta
any(is.na(new_data))
sum(is.na(new_data))

which(colSums(is.na(new_data)) > 0) #eliminar variaveis que possuem elevado número de NA, nomeadamente, as que estao associadas a datas e horas
data <- new_data[,!names(new_data) %in% c("Data", "Hora", "AC0", "AC1", "AC2_3", "AC_mais_q_3", "AC_1_todos", "AC_2_3_todos")]
missmap(data)
data <- data[,!names(data) %in% c("HCO3...30")]
missmap(data, x.cex = 0.55)   

# verificar se estas variaveis possuem os mesmos valores (variaveis duplicadas)
identical(data[['SIRS...19']], data[['SIRS...31']])
identical(data[['SIRS...19']], data[['SIRS...88']])  # posso eliminar 2 delas

# eliminar variaveis desnecessarias e redundantes
sirs <- c("SIRS...31", "SIRS...88", "SIRS_2")
data <- data[,!names(data) %in% sirs]

#corrigir o nome das variaveis
data <- data %>% rename("SIRS" = "SIRS...19")
data <- data %>% rename("RASS" = "RASS...38")
data <- data %>% rename("HCO3" = "HCO3...29")
data <- data %>% rename("Alverine" = "Alverine_1")
data <- data %>% rename("Alprazolam" = "Alprazolam_1")
data <- data %>% rename("Captopril" = "Captopril_1")
data <- data %>% rename("Codeine" = "Codeine_1")
data <- data %>% rename("Desloratadine" = "Desloratadine_1")
data <- data %>% rename("Diazepam" = "Diazepam_1")
data <- data %>% rename("Digoxin" = "Digoxin_1")
data <- data %>% rename("Dipyridamole" = "Dipyridamole_1")
data <- data %>% rename("Furosemide" = "Furosemide_1")
data <- data %>% rename("Fluvoxamine" = "Fluvoxamine_1")
data <- data %>% rename("Haloperidol" = "Haloperidol_1")
data <- data %>% rename("Hydrocortisone" = "Hydrocortisone_1")
data <- data %>% rename("Iloperidone" = "Iloperidone_1")
data <- data %>% rename("Morphine" = "Morphine_1")
data <- data %>% rename("Nifedipine" = "Nifedipine_1")
data <- data %>% rename("Paliperidone" = "Paliperidone_1")
data <- data %>% rename("Prednisone" = "Prednisone_1")
data <- data %>% rename("Ranitidine" = "Ranitidine_1")
data <- data %>% rename("Risperidone" = "Risperidone_1")
data <- data %>% rename("Trazodone" = "Trazodone_1")
data <- data %>% rename("Venlafaxine" = "Venlafaxine_1")
data <- data %>% rename("Warfarin" = "Warfarin_1")
data <- data %>% rename("Amitriptyline" = "Amitriptyline_3")
data <- data %>% rename("Paroxetine" = "Paroxetine_3")
data <- data %>% rename("Hydroxyzine" = "Hydroxyzine_3")
data <- data %>% rename("Quetiapine" = "Quetiapine_3")
data <- data %>% rename("Scopolamine" = "Scopolamine_3")
data <- data %>% rename("Trihexyphenidyl" = "Trihexyphenidyl_3")
data <- data %>% rename("Mexazolam_sedoxil" = "Mexazolam..sedoxil.")
data <- data %>% rename("Trospium" = "Trospium_3")
data <- data %>% rename("Result" = "resulta_hipo_hiper")



proveniencia <- c("casa", "lar", "inter_hospitalar", "intra_hospitalar")
local_su <- c("UDC1", "UDC2", "UCISU","AMBUL")
grupo_diagnostico <- c("neuro", "cardio", "gastro", "respiratotio", "genito", "musculo_Esq", "toxi_drog", "outro", "hemato")
crit_sirs <- c("Freq_Card", "Freq_Resp", "Temp_Corporal", "Cont_Leucocitos")
crit_rass <- c("Falta_atenção_auditiva", "Falta_atenção_visual", "RASS...90", "RASS_3")

data <- data[, !names(data) %in% c("N.Proc", proveniencia, local_su, grupo_diagnostico, crit_sirs, crit_rass, "ResultDelirium", "RASS_2", "PrimaryFirst")]
missmap(data)

# verificar se exitem colunas e linhas duplicadas
table(duplicated(data))  # nao existem linhas duplicadas
unique(data)

# analisar as variaveis e fazer correçoes necessarias
data$RASS[data$RASS == '15'] <- NA   # trocar 15 por NA
sum(is.na(data$RASS))   #67
boxplot(data$RASS)
barplot(table(data$RASS))

# RASS valores negativos = -1 (sedação), valores positivos = 1 (agitação), valores de 0 = 0 (calmo)
data$RASS[data$RASS < 0] <- -1
data$RASS[data$RASS > 0] <- 1
table(data$RASS)    
barplot(table(data$RASS), main = c("RASS"))     


#ver qual faz mais sentido, internamento em horas ou dias
range(data$Interna_Horas)
mean(data$Interna_Horas)
data <- data[,!names(data) %in% "Interna_Horas"]
data <- data[,!names(data) %in% c("Antihistaminico", "Antidepressivo", "Antipsicotico", "Anticolinerg_Central", "Antiespasmodicos_GInt", "Antiespasmodico_GUrin", "Relaxante_Musc", "Analgesico", "Antiepileptico","Antihemetico")]

list_unique <- lapply(data, unique)  #lista com valores unicos para cada vari?vel
var <- lapply(data, var)
#codeine e alverine so tem um valor unico, eliminar isto, variancia = 0
dados <- data[,!names(data) %in% c("Alverine", "Codeine")]


dados$Antihipertensivos <- rowSums(dados[ , c("Furosemide", "Captopril", "Clonidine", "Nifedipine")])
dados$Antidislipidemicos <- rowSums(dados[ , c("Rosuvastatina", "Atorvastatina", "Pravastatina", "Sinvastatina", "Fluvastatina")])
dados$Ansioliticos <- rowSums(dados[ , c("Alprazolam", "Diazepam", "Lorazepam", "Mexazolam_sedoxil")])
dados$Analgesicos <- rowSums(dados[ , c("Morphine", "Tramadol")])
dados$Antidepressivos <- rowSums(dados[ , c("Fluvoxamine", "Trazodone", "Venlafaxine", "Amitriptyline", "Paroxetine", "Sertralina")])
dados$Antipsicoticos <- rowSums(dados[ , c("Iloperidone", "Paliperidone", "Risperidone", "Quetiapine", "Haloperidol")])
dados$Anticoagulantes <- rowSums(dados[ , c("Warfarin")])
dados$Antiagregantes <- rowSums(dados[ , c("Dipyridamole")])
dados$Corticosteroides <- rowSums(dados[ , c("Hydrocortisone", "Prednisone")])
dados$Cardiotonicos <- rowSums(dados[ , c("Digoxin")])
dados$Antiparkinsonicos <- rowSums(dados[ , c("Trihexyphenidyl")])
dados$Antiacidos <- rowSums(dados[ , c("Ranitidine")])
dados$Antihistaminicos <- rowSums(dados[ , c("Hydroxyzine", "Desloratadine")])
dados$Antiespasmodicos <- rowSums(dados[ , c("Trospium", "Scopolamine")])


dados <- dados[, !names(dados) %in% c("Digoxin", "Trihexyphenidyl", "Ranitidine", "Trospium", "Scopolamine", "Hydroxyzine", "Desloratadine", "Hydrocortisone", "Prednisone", "Dipyridamole", "Warfarin", "Furosemide", "Captopril", "Clonidine", "Nifedipine", "Rosuvastatina", "Atorvastatina", "Pravastatina", "Sinvastatina", "Fluvastatina", "Alprazolam", "Diazepam", "Lorazepam", "Mexazolam_sedoxil", "Morphine", "Tramadol", "Iloperidone", "Paliperidone", "Risperidone", "Quetiapine", "Haloperidol","Fluvoxamine", "Trazodone", "Venlafaxine", "Amitriptyline", "Paroxetine", "Sertralina")]


#grupos farmacologicos com representatividade inferior a 10/14 pacientes foram colocados na categoria de Outros medicamnetos
table(dados$Antihipertensivos)
table(dados$Analgesicos)
table(dados$Antidislipidemicos)
table(dados$Ansioliticos)
table(dados$Antidepressivos)
table(dados$Antipsicoticos)
table(dados$Anticoagulantes)
table(dados$Antiagregantes)    #OUTRO
table(dados$Antiacidos)        #OUTRO
table(dados$Antiparkinsonicos) #OUTRO
table(dados$Antihistaminicos)  #OUTRO
table(dados$Antiespasmodicos)  #OUTRO
table(dados$Corticosteroides)
table(dados$Cardiotonicos)

sum(dados$Farmaco[dados$Result == "Hipoativo"])

dados$Outros_Med = dados$Antiacidos + dados$Antiparkinsonicos + dados$Antiespasmodicos + dados$Antihistaminicos + dados$Antiagregantes
dados <- dados[, !names(dados) %in% c("Antiacidos", "Antiparkinsonicos", "Antiespasmodicos", "Antihistaminicos", "Antiagregantes")]

database <- dados %>% relocate(c(Alcoolico, Data_Obito, Result), .after = Outros_Med)

# scatterplot
plot(Result ~ Idade, database)
p <- ggplot(filter(database),
            aes(x = Idade, y = Result)) 
p <- p + scale_x_log10() 
p + geom_point() 
p + geom_point(alpha = (1/3), size = 3) + geom_smooth(lwd = 3, se = FALSE)
p + geom_point(alpha = (1/3), size = 3)  + geom_smooth(lwd = 1.5, se = FALSE)

write_xlsx(database, "C:/Users/ASUS/OneDrive/final_database.xlsx")

#434,31

# normalizar os dados

db <- read_excel("C:/Users/ASUS/OneDrive/final_database.xlsx")

db_f <- preProcess(database[,8:17], method=c("range"))

db_normal <- predict(db_f, as.data.frame(db))

write_xlsx(db_normal, "C:/Users/ASUS/OneDrive/db_normalized.xlsx")

############## Análise exploratória ########################### 

library(grid)
dfSummary(data, na.col=F,valid.col=F)

grupos_idade <- cut(new_data$Idade, breaks = c(seq(17, 100, by = 10)), include.lowest = TRUE, labels = FALSE)
grupos_idade <- paste(grupos_idade * 10 - 9, "-", grupos_idade * 10, sep = "")
dad <- data.frame(idade = grupos_idade, subtipo_delirium = new_data$Result)
contagem <- table(dad)

plot <- barchart(contagem, horizontal = FALSE, col = c("#98D8D8", "#FFDAB9", "#E6A8D7"), xlab = "Age (years)", ylab = "Number of patients")

cores <- c("#98D8D8", "#FFDAB9", "#E6A8D7")

legend_labels <- unique(new_data$Result)

legend_colors <- cores

# Criar a posição e tamanho da legenda
legend_x <- "right"
legend_y <- "top"
legend_width <- 5
legend_height <- 5

# Plot o grafico de barras e a legenda
grid.newpage()
pushViewport(plotViewport(margins = rep(0.2, 4)))

print(plot, vp = current.viewport())

# Função para desenhar os retangulos da legenda
draw_legend_rectangles <- function(x, y, width, height, colors) {
  for (i in seq_along(colors)) {
    grid.rect(x = x, y = y - i * height, width = width, height = height,
              gp = gpar(fill = colors[i], col = NA))
  }
}

# Função para adicionar o texto da legenda
add_legend_labels <- function(x, y, labels) {
  for (i in seq_along(labels)) {
    grid.text(label = labels[i], x = x, y = y - i * height,
              gp = gpar(fontsize = 12, col = "black"))
  }
}

# Desenhar a legenda
pushViewport(viewport(x = 8, y = 9, width = legend_width, height = legend_height))

draw_legend_rectangles(x = 0.7, y = 1, width = 0.1, height = 0.3, colors = legend_colors)
add_legend_labels(x = 0.8, y = 1, labels = legend_labels)

popViewport(1)


library(corrplot)
X <- as.matrix(train_data[,c(8:27)])
corrplot::corrplot(cor(X))


# analisar colinearidade, se VIF >10 tem problemas de colinearidade
full <- lm(Result ~ ., data=train_data)
olsrr::ols_vif_tol(full)  
