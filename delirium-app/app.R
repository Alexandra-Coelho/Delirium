# Pacotes
library(shiny)
library(nnet)
library(dplyr)
library(readxl)
library(caret)
library(MASS)
library(shinydashboard)
library(shinyBS)

addResourcePath("prefix", "www")

title <- tags$a(icon("brain"), "Delirium Detection App", target="_blank", style= 'color: white')

ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(title = title, titleWidth = 800),
    
    dashboardSidebar(
      sidebarMenu(
      menuItem("Página Inicial",
               tabName = "pag_inicial_tab",
               icon = icon("home")),
      menuItem("Sobre",
               tabName = "sobre_tab",
               icon = icon("circle-info")),
      menuItem("Formulário",
               tabName = "forms_tab",
               icon = icon("clipboard-list")),
      menuItem("Confirmação de dados",
               tabName = "list",
               icon = icon("list-check")),
      menuItem("Resultado",
               tabName = "result_tab",
               icon = icon("chart-line"))
    )),
  
  
  
  
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom1.css")),

    tabItems(
      tabItem(tabName = "pag_inicial_tab",
              tags$img(src = "prefix/pag.png", align='center', width=1300, height=800)),   #acrescentar imagem
      
      tabItem(tabName = "sobre_tab", 
              
      h2("Autor"),
      
      p("A aplicação foi desenvolvida por ", strong("Alexandra Coelho, "), "sob orientação da ", strong("Professora Doutora Ana Cristina Braga "), "e coorientação do ", strong('Doutor José Mariz'),".
        A sua construção fez parte da dissertação do Mestrado em Bioinformática da Universidade do Minho."),
      
      br(),
      h2("Finalidade da aplicação"),
              
      p("O ", em("delirium"), "é uma disfunção neuropsiquiátrica aguda, sendo clinicamente caraterizado pelo défice de atenção e turvação do estado de consciência associado a distúrbios comportamentais e cognitivos. 
        Adicionalmente, o ", em("delirium"), "pode ser categorizado consoante o perfil de atividade motora em: hipoativo e hiperativo."),
      
      p("Esta aplicação tem como objetivo auxiliar os profissionais de saúde no diagnóstico dos diferentes subtipos de ", em("delirium"), "em indivíduos admitidos no contexto de serviço de urgência.
        Nestas circunstâncias, o tempo é essencial, bem como o correto e precoce diagnóstico para intervir o mais rapidamente possível. 
        Assim, urge a necessidade de uma ferramenta eficaz e capaz de prever a ocorrência de ", em("delirium"), "e dos seus subtipos, com o intuito de minimizar as consequências nefastas desta doença."),
      
      br(),
      h2("Modelo de previsão implementado na aplicação"),  #style="font-weight: bold; font-size:24px"
      
      p("O algoritmo de classificação implementado nesta aplicação trata-se de um modelo de regressão logística multinomial, o qual apresenta uma ", strong('accuracy'), " de 0,638 e um valor de AUC-ROC de 0,691 e 0,531 para o subtipo hipoativo e hiperativo, respetivamente. 
        Este modelo preditivo aplicou o método ",em("embedded"), "de Elastic Net com um valor de ", em("alpha"), "de 0,1, resultando num modelo constituído por 26 variáveis independentes. "),
      
      br(),
      h2("Ferramentas utilizadas"),
      
      p("Esta aplicação foi desenvolvida utilizando a linguagem de programação ", a("R", href = "https://www.r-project.org/about.html"),"(versão 4.1.2), 
        o ambiente ", a("RStudio", href = "https://posit.co/products/open-source/rstudio/"), "(versão 2023.06.1+524) e o pacote ", a("Shiny", href = "https://shiny.posit.co"), "(versão 1.7.5)."),
      br(),
      splitLayout(cellWidths = c("13%", "25%", "14%"), cellArgs = list(style = "padding: 0px"), style = "border: 0px; padding: 0px;",
      tags$img(src = "prefix/R.png", width=100, height=100),
      tags$img(src = "prefix/rstudio.png", width=250, height=100),
      tags$img(src = "prefix/shiny.png", width=100, height=100)),
      
      br(),
      h2("Contactos"),
      p("Caso persista alguma dúvida, não hesite em contactar o responsável pelo desenvolvimento da aplicação, Alexandra Coelho, através do email ", strong("pg45458@alunos.uminho.pt", "."))
      ),
      
      
    
    
  tabItem(tabName= "forms_tab", h2("Formulário"), helpText("Preencha todos os dados requeridos abaixo para se proceder à previsão dos subtipos de delirium."),
  
  column(width=3,
  radioButtons("Genero", "Selecione o género:", list("Feminino", "Masculino"), ""),     
  
  sliderInput("Idade", "Idade", min=18, max=100, value = 66), 
  
  selectInput(inputId = "Local_SU", label = "Estado do paciente:",
              choices = c("Ambulatorio", "UCI" ,"UDC1", "UDC2")),
  
  numericInput(inputId = "Interna_Dias", label = "Tempo de permanência no SU (horas)", value = 22, min=2, max=288),
           
  selectInput(inputId = "Proveniencia", label = "Local de Proveniência:",
              choices = c("Casa", "Lar","Intra-Hospitalar", "Inter-Hospitalar")),
  
  selectInput(inputId = "Grupo_Diagn", label = "Grupo de Diagnóstico:",
              choices = c("Hemato-oncologico", "Neurologico", "Respiratorio", "Cardiovascular", "Musculo-Esqueletico", "Geniturinario", "Gastrointestinal", "Outro")),
  
  radioButtons("Alcoolico", "Consome álcool em excesso?", list("Sim", "Nao"), "")
  
  ), 
  
  column(width=3,
         sliderInput("SIRS", label = tags$span("Critérios SIRS", 
                                               tags$i(
                                                 class = "glyphicon glyphicon-info-sign", 
                                                 style = "color:#0072B2;",
                                                 title = "
  São um método de avaliação inicial para identificar os pacientes sujeitos a um processo inflamatório agudo ou infeção, quando pelo menos
  dois critérios são atendidos.
    Temperatura >38 ºC ou <36 ºC
    Frequência respiratória >20 rpm ou pCO2 <32 mmHg
    Frequência cardáaca >90 bpm
    Glóbulos brancos >12.000 cel/mm^3 ou <4000 cel/mm^3"
                                                 )), min=0, max=4, value = 0),
  numericInput(inputId = "Glicose", label = "Glicose (mg/dL):", value = 151, min=41, max=1000),
  numericInput(inputId = "Sodio", label = "Sódio (mEq/L):", value = 138, min=42, max=151),
  numericInput(inputId = "Ureia", label = "Ureia (mg/dL):", value = 62 , min = 4, max = 275),
  numericInput(inputId = "Creatinina", label = "Creatinina (mg/dL):", value = 1.5 , min = 0.1, max = 19.5),
  numericInput(inputId = "PCR", label = "PCR (mg/L):", value = 49.4 , min = 2.9, max = 499),
  numericInput(inputId = "pH", label = "pH:", value = 7.4 , min = 7.026, max = 7.625),
  numericInput(inputId = "Ca_ionizado", label = "Cálcio ionizado (mmol/L):", value = 1.16 , min = 0.84, max = 1.37)
  ),

  
  column(width=3,
  
  #selectInput(inputId = "Antihipertensivos", label = "Antihipertensivos:",
              #choices = c("","Furosemida", "Captopril", "Clonidina", "Nifedipina"), multiple = TRUE),
  
  numericInput(inputId = "pO2", label = "Pressão parcial de oxigénio (mm Hg):", value = 86.6 , min = 34.1, max = 178.1),
  numericInput(inputId = "HCO3", label = "Ião bicarbonato (mEq/L):", value = 24.5 , min = 7.4, max = 39.1),
  
  selectInput(inputId = "Antidislipidemicos", label = "Antidislipidémicos:",
              choices = c("","Rosuvastatina", "Atorvastatina", "Pravastatina", "Sinvastatina", "Fluvastatina"), multiple = TRUE),
  
  selectInput(inputId = "Ansioliticos", label = "Ansiolíticos:",
              choices = c("","Alprazolam", "Diazepam", "Lorazepam", "Mexazolam_sedoxil"), multiple = TRUE),
  
  selectInput(inputId = "Analgesicos", label = "Analgésicos:",
              choices = c("","Morfina", "Tramadol"), multiple = TRUE),
  
  selectInput(inputId = "Antidepressivos", label = "Antidepressivos:",
              choices = c("","Fluvoxamina", "Trazodona", "Venlafaxina", "Amitriptilina", "Paroxetina", "Sertralina"), multiple = TRUE),
  
  selectInput(inputId = "Antipsicoticos", label = "Antipsicóticos:",
              choices = c("","Iloperidona", "Paliperidona", "Risperidona", "Quetiapina", "Haloperidol"), multiple = TRUE),
  
  selectInput(inputId = "Anticoagulantes", label = "Anticoagulantes:",
              choices = c("","Varfarina"))),
  
  column(width=3,
  
  selectInput(inputId = "Corticosteroides", label = "Corticosteróides:",  choices = c("","Hidrocortisona", "Prednisona"), multiple = TRUE),
  
  selectInput(inputId = "Cardiotonicos", label = "Cardiotónicos:",
              choices = c("","Digoxina")),
  
  selectInput(inputId = "Outros_Med", label = tags$span("Outros Medicamentos:", 
              tags$i(
                class = "glyphicon glyphicon-info-sign", 
                style = "color:#0072B2;",
                title = "Agrega medicamentos de diversos grupos farmacológicos como antiagregantes, antiácidos, antiparkinsónicos, antihistamínicos e antiespasmódicos."
              )),
              choices = c("","Dipiridamol", "Ranitidina", "Trihexifenidilo", "Hidroxizina", "Desloratadina", "Cloreto de Trospio", "Escopolamina"), multiple = TRUE)
  )),
  

  
  tabItem(tabName="list", p("Confirme se introduziu os dados corretamente."),
  
  div(style = "overflow-x: auto;", tableOutput("inputTable") ),
  
  # Button to perform regression
  actionButton(inputId = "btn_run_regression", label = "Estimar", icon = icon("arrows-rotate"))),
  
  tabItem(tabName="result_tab", helpText("Resultado da previsão do risco de desenvolvimento de um dos subtipos de delirium:", style= 'color: black; font-weight: bold'),
  # Output: Display regression results
  verbatimTextOutput(outputId = "output_result"))
)))  ) 


server <- function(input, output) {
  
  # Carrega o dataset de treino
  train_dataset <- read_xlsx("Train_adas.xlsx")
  
    
  output$inputTable <- renderTable({ data.frame(Genero = input$Genero, Idade = input$Idade, Local_SU = input$Local_SU, Tempo_SU = input$Interna_Dias, Proveniencia = input$Proveniencia, Grupo_Diagnostico = input$Grupo_Diagn, Alcoolico = input$Alcoolico, 
                                                  SIRS = input$SIRS, Glicose = input$Glicose, Sódio = input$Sodio, Ureia = input$Ureia, Creatinina = input$Creatinina, PCR = input$PCR, pH = input$pH, Ca_ionizado = input$Ca_ionizado, pO2 = input$pO2,
                                                  HCO3 = input$HCO3,  Antidislipidemicos = paste(input$Antidislipidemicos, collapse = ", "), Ansioliticos = paste(input$Ansioliticos, collapse = ", "), Analgesicos = paste(input$Analgesicos, collapse = ", "), Antidepressivos = paste(input$Antidepressivos, collapse = ", "),
                                                  Antipsicoticos = paste(input$Antipsicoticos, collapse = ", "), Anticoagulantes = input$Anticoagulantes, Corticosteroides = paste(input$Corticosteroides, collapse = ", "), Cardiotonicos = input$Cardiotonicos, Outros_Medicamentos = paste(input$Outros_Med, collapse = ", "))}, options = list(scrollX = TRUE) ) #})
  
  
  # Implementa a regressao logistica multinomial 
  observeEvent(input$btn_run_regression, {
    
    # Formula para a regressao logistica multinomial 
    formula <- as.formula("Result ~ pO2 + HCO3 + pH + PCR + Creatinina + Ureia + Ca_ionizado + Sodio + Glicose + Proveniencia + SIRS + Grupo_Diagn + Genero +
                          Antidislipidemicos + Ansioliticos + Analgesicos + Antidepressivos + Antipsicoticos + Anticoagulantes + Corticosteroides + Cardiotonicos + Outros_Med + Alcoolico + Idade + Local_SU + Interna_Dias")
    
    # Cria um data frame com os valores introduzidos pelo utilizador
    selected_data <- data.frame(
      Result = NA,  # Placeholder para a variavel dependente, vai ser reescrita
      pO2 = input$pO2, Creatinina = input$Creatinina, HCO3 = input$HCO3, pH = input$pH, PCR = input$PCR, Ureia = input$Ureia, Ca_ionizado = input$Ca_ionizado,
      Glicose = input$Glicose, Sodio = input$Sodio, Genero = input$Genero, Proveniencia = input$Proveniencia, Grupo_Diagn = input$Grupo_Diagn, Antidislipidemicos = length(input$Antidislipidemicos),
      Analgesicos = length(input$Analgesicos), Alcoolico = input$Alcoolico, Idade = input$Idade, Local_SU = input$Local_SU, Ansioliticos = length(input$Ansioliticos), Antidepressivos = length(input$Antidepressivos),
      Interna_Dias = 0.04166667*(input$Interna_Dias),  # converte horas -> dias
      SIRS = input$SIRS, Antipsicoticos = length(input$Antipsicoticos), Anticoagulantes = length(input$Anticoagulantes), Corticosteroides = length(input$Corticosteroides), Cardiotonicos = length(input$Cardiotonicos), Outros_Med = length(input$Outros_Med)
    )
    
    initial_model <- multinom(formula, data = train_dataset)
    model <- stepAIC(initial_model, direction = "backward")
    
    # Implementa o modelo de regressao logistica multinomial utilizando o dataset de treino
    model <- multinom(formula, data = train_dataset)
    
    # Prevê as categorias 
    selected_data$Result <- predict(model, newdata = selected_data, type = "class")
    
    # Apresentaçao dos resultados
    output$output_result <- renderText({
      if (selected_data$Result == "Ausente") 
        {return('É provável que o indivíduo não apresente delirium.')} 
      else if (selected_data$Result == "Hipoativo") 
        {return('É provável que o indivíduo apresente o subtipo hipoativo.')}
      else {
        return('É provável que o indivíduo apresente o subtipo hiperativo.')}
      
    })
  })
}



shinyApp(ui=ui, server=server)