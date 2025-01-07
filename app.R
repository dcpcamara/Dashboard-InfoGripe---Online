# Load necessary library
library(shiny)
library(stringi)
library(tidyverse)
library(vroom)

####Baixando os dados

path = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv"
dados <- vroom(file = path)

path_obt = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/obitos_semanais_fx_etaria_virus_sem_filtro_febre.csv"
dados_obt <- vroom(file = path_obt)


###Baixando os dados

# Define the UI for the application
ui <- fluidPage(
  # Add custom CSS to ensure images do not exceed the window size
  tags$style(HTML("
        .responsive-img {
            max-width: 100%;
            max-height: 500px;
            width: auto;
            height: auto;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }
        .logo-banner {
            max-height: 50px;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }
        .title-panel {
            text-align: center;
            font-size: 24px;
            font-weight: bold;
            padding-left: 20px;
            display: inline-block;
        }
        .figure-title {
            text-align: center;
            margin-left: 15px; 
            margin-top: 40px;
        }
        .figure-space {
            margin-bottom: 60px; /* Add space between figures */
        }
    ")),
  
  # Include the logo and title in the same row
  fluidRow(
    column(1, 
           img(src = "logo1.png", 
               class = "logo-banner")),  # Logo in the first column
    column(1, 
           img(src = "logo2.png", 
               class = "logo-banner")),  # Second logo in the second column
    column(10, 
           div("Dashboard de resultados do boletim InfoGripe", 
               class = "title-panel"))  # Title in the second column
  ),
  
  # Create a tab layout
  tabsetPanel(
    # First Tab: Trends
    tabPanel("Tendências",
             # Two figures vertically with titles
             h3("Mapa de tendência de SRAG para o curto e longo prazo - Unidades Federativas", 
                class = "figure-title"),
             fluidRow(
               uiOutput("trends_figure1")
             ),
             h3("Mapa de tendência de SRAG para o curto e longo prazo - somente capitais", 
                class = "figure-title"),
             fluidRow(
               uiOutput("trends_figure2")
             )
    ),
    
    # Second Tab: Brazil
    tabPanel("Brasil",
             # Figures 1 and 2 side by side with a header
             h3("Figura 1. Série temporal, estimativa de casos recentes de SRAG e tendências de curto e longo prazo em todo o território nacional - geral e por faixas etárias de interesse.", 
                class = "figure-title"),
             fluidRow(
               column(6, 
                      uiOutput("brazil_figure1")),
               column(6, 
                      uiOutput("brazil_figure2"))
             ),
             
             # Figure 3 alone with a header
             h3("Figura 2. Incidência e mortalidade nas últimas 8 semanas.", 
                class = "figure-title"),
             fluidRow(
               column(12, 
                      uiOutput("brazil_figure3"))
             ),
             
             # Figures 4 and 5 side by side with a header
             h3("Figura 3. Média da incidência e mortalidade semanal de SRAG nas últimas oito semanas, por vírus e faixa etária de interesse.",
                class = "figure-title"),
             fluidRow(
               column(6, 
                      uiOutput("brazil_figure4")),
               column(6, 
                      uiOutput("brazil_figure5"))
             ),
             
             # Figure 6 alone at the bottom
             h3("Figura 4. Incidência semanal de SRAG e por vírus identificado laboratorialmente, por faixas etárias de interesse.", 
                class = "figure-title"),
             fluidRow(
               column(12, 
                      uiOutput("brazil_figure6"))
             )
    ),
    
    # Third Tab: Brazilian States
    tabPanel("Unidades Federativas",
             # Dropdown menu for selecting a state
             selectInput("selected_state", "Selecione uma Unidade Federativa:",
                         choices = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", 
                                     "ES", "GO", "MA", "MT", "MS", 
                                     "MG", "PA", "PB", "PR", "PE", "PI", "RJ", 
                                     "RN", "RS", "RO", "RR", "SC", 
                                     "SP", "SE", "TO")),
             
             # Titles and images
             h3("Figura 1. Série temporal, estimativa de casos recentes de SRAG e tendências de curto e longo prazo por Unidade Federativa.", 
                class = "figure-title"),
             fluidRow(
               column(6, 
                      uiOutput("state_image1")),
               column(6, 
                      uiOutput("state_image2"))
             ),
             h3("Figura 2. Casos semanais de SRAG por vírus identificado.", 
                class = "figure-title"), 
             fluidRow(
               uiOutput("state_image3")
             )
    ),
    
    # Fourth Tab: Brazilian Capitals
    tabPanel("Capitais",
             # Dropdown menu for selecting a capital
             selectInput("selected_capital", "Selecione uma Capital:",
                         choices = sort(c("Aracaju", "Belém", "Belo Horizonte", "Boa Vista", "Brasília", "Campo Grande", 
                                          "Cuiabá", "Curitiba", "Florianópolis", "Fortaleza", "Goiânia", "João Pessoa", 
                                          "Macapá", "Maceió", "Manaus", "Natal", "Palmas", "Porto Alegre", 
                                          "Porto Velho", "Recife", "Rio Branco", "Rio de Janeiro", "Salvador", 
                                          "São Luís", "São Paulo", "Teresina", "Vitória"))),
             
             # Display the figures with header titles
             h3("Série temporal, estimativa de casos recentes de SRAG e tendências por Capital.", 
                class = "figure-title"),
             fluidRow(
               column(6, 
                      uiOutput("capital_image1")),
               column(6, 
                      uiOutput("capital_image2"))
             )
    ),
    
    # 5th Tab: Cases per virus
    tabPanel("Tabelas",
             # Dropdown menu for selecting a capital
             
          column(6, selectInput(inputId = "selected_state", 
                          label ="Selecione um estado:",
                           choices = c("BR", "AC", "AL", "AP", "AM", "BA", "CE", "DF", 
                                          "ES", "GO", "MA", "MT", "MS", 
                                          "MG", "PA", "PB", "PR", "PE", "PI", "RJ", 
                                          "RN", "RS", "RO", "RR", "SC", 
                                          "SP", "SE", "TO")
                                          )
                         ),
          
          column(6, selectInput(inputId = "selected_age", 
                             label ="Selecione uma Faixa Etária:",
                             choices = c("Total", "< 2", "65+")
                             )
          ),
             
             # Display the figures with header titles
             h3("Tabela de casos e óbitos de SRAG por agente etiológico", 
                class = "table-title"),
             
             fluidRow(
               column(12,
                      DT::dataTableOutput('tabela_resumo'))
               )
             )

)

)


# Define server logic
server <- function(input, output) {
  
  ### Tab 1: Trends Figures
  output$trends_figure1 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/Mapa_ufs_tendencia.png", 
             class = "responsive-img", 
             alt = "Trends Figure 1")
  })
  
  output$trends_figure2 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/Capitais/Mapa_capitais_tendencia.png", 
             class = "responsive-img", 
             alt = "Trends Figure 2")
  })
  
  ### Tab 2: Brazil Static Figures
  output$brazil_figure1 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR.png", 
             class = "responsive-img", 
             alt = "Brazil Figure 1")
  })
  
  output$brazil_figure2 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR_fx_etaria.png", 
             class = "responsive-img", 
             alt = "Brazil Figure 2")
  })
  
  output$brazil_figure3 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR_inc_mort.png", 
             class = "responsive-img", 
             alt = "Brazil Figure 3")
  })
  
  output$brazil_figure4 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR_virus_lab_inc.png", 
             class = "responsive-img", 
             alt = "Brazil Figure 4")
  })
  
  output$brazil_figure5 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR_virus_lab_mort.png", 
             class = "responsive-img", 
             alt = "Brazil Figure 5")
  })
  
  output$brazil_figure6 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR_virus_lab.png", 
             class = "responsive-img", 
             alt = "Brazil Figure 6")
  })
  
  ### Tab 3: Brazilian States
  get_image_url <- function(state, image_number) {
    base_url <- "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/"
    if (image_number == 1) {
      return(paste0(base_url, "fig_", state, ".png"))
    } else if (image_number == 2) {
      return(paste0(base_url, "fig_", state, "_fx_etaria.png"))
    } else if (image_number == 3) {
      return(paste0(base_url, "fig_", state, "_virus_lab.png"))
    }
  }
  
  output$state_image1 <- renderUI({
    state <- input$selected_state
    tags$img(src = get_image_url(state, 1), 
             class = "responsive-img", 
             alt = paste("UF", state, "Image 1"))
  })
  
  output$state_image2 <- renderUI({
    state <- input$selected_state
    tags$img(src = get_image_url(state, 2), 
             class = "responsive-img", 
             alt = paste("UF", state, "Image 2"))
  })
  
  output$state_image3 <- renderUI({
    state <- input$selected_state
    tags$img(src = get_image_url(state, 3), 
             class = "responsive-img", 
             alt = paste("UF", state, "Image 3"))
  })
  
  ### Tab 4: Brazilian Capitals
  get_capital_image_url <- function(capital, image_number) {
    # Map capital names to state abbreviations
    state_abbr <- switch(
      capital,
      "Rio Branco" = "AC", "Maceió" = "AL", "Macapá" = "AP", "Manaus" = "AM",
      "Salvador" = "BA", "Fortaleza" = "CE", "Brasília" = "DF", "Vitória" = "ES",
      "Goiânia" = "GO", "São Luís" = "MA", "Cuiabá" = "MT", "Campo Grande" = "MS",
      "Belo Horizonte" = "MG", "Belém" = "PA", "João Pessoa" = "PB", "Curitiba" = "PR",
      "Recife" = "PE", "Teresina" = "PI", "Rio de Janeiro" = "RJ", "Natal" = "RN",
      "Porto Alegre" = "RS", "Porto Velho" = "RO", "Boa Vista" = "RR", "Florianópolis" = "SC",
      "São Paulo" = "SP", "Aracaju" = "SE", "Palmas" = "TO"
    )
    
    base_url <- "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/Capitais/"
    capital_formatted <- gsub(" ", 
                              "_", 
                              toupper(stri_trans_general(capital, "Latin-ASCII")))
    
    if (image_number == 1) {
      return(paste0(base_url, "fig_", state_abbr, "_", capital_formatted, ".png"))
    } else if (image_number == 2) {
      return(paste0(base_url, "fig_", state_abbr, "_", capital_formatted, "_fx_etaria.png"))
    }
  }
  
  output$capital_image1 <- renderUI({
    capital <- input$selected_capital
    tags$img(src = get_capital_image_url(capital, 1), 
             class = "responsive-img", 
             alt = paste("Capital", capital, "Image 1"))
  })
  
  output$capital_image2 <- renderUI({
    capital <- input$selected_capital
    tags$img(src = get_capital_image_url(capital, 2), 
             class = "responsive-img", 
             alt = paste("Capital", capital, "Image 2"))
  })
  
  ###Tab 5
  
  tabela_download <- reactive({
    
    ano<- dados %>% filter(epiyear=="2024")
    
    dt_srag <- dados %>% filter(DS_UF_SIGLA==input$selected_state) %>%
      filter(epiyear==2024, fx_etaria==input$selected_age)
    
    dt_obt <- dados_obt %>% filter(DS_UF_SIGLA==input$selected_state) %>%
      filter(epiyear==2024, fx_etaria==input$selected_age)
    
    max_week<-max(ano$epiweek)-4
    
    
    tabela_srag<- dt_srag %>%
      summarise("SRAG (n)"=sum(SRAG),
                Positivos=sum(positivos),
                "Positivos (n)"=sum(positivos),
                "Flu A %"=round((sum(FLU_A)*100)/Positivos,1),
                "Flu_B %"=round((sum(FLU_B)*100)/Positivos,1),
                "Covid-19 %"=round(sum(SARS2)*100/Positivos),
                "VSR %"=round((sum(VSR)*100)/Positivos,1),
                "Rino %"=round((sum(RINO)*100)/Positivos,1),
                "Aden %"=round((sum(ADNO)*100)/Positivos,1),
                "Meta %"=round((sum(METAP)*100)/Positivos,1),
                "Boca %"=round((sum(BOCA)*100)/Positivos,1),
                "Paraflu %"=round((sum(PARA1, PARA2, PARA3, PARA4)*100)/Positivos,1),
                
      ) %>% mutate(Período="2024", .before = "SRAG (n)") %>%
      mutate(Dados="Casos", .before = "Período") %>%
      select(!Positivos)
      
    
    
    tabela_srag_4semanas<- dt_srag %>%
      filter(epiweek>max_week) %>%
      summarise("SRAG (n)"=sum(SRAG),
                Positivos=sum(positivos),
                "Positivos (n)"=sum(positivos),
                "Flu A %"=round((sum(FLU_A)*100)/Positivos,1),
                "Flu_B %"=round((sum(FLU_B)*100)/Positivos,1),
                "Covid-19 %"=round(sum(SARS2)*100/Positivos),
                "VSR %"=round((sum(VSR)*100)/Positivos,1),
                "Rino %"=round((sum(RINO)*100)/Positivos,1),
                "Aden %"=round((sum(ADNO)*100)/Positivos,1),
                "Meta %"=round((sum(METAP)*100)/Positivos,1),
                "Boca %"=round((sum(BOCA)*100)/Positivos,1),
                "Paraflu %"=round((sum(PARA1, PARA2, PARA3, PARA4)*100)/Positivos,1),
                
      ) %>% mutate(Período="4 semanas", .before = "SRAG (n)") %>%
      mutate(Dados="Casos", .before = "Período") %>%
      select(!Positivos)
    
    
    tabela_srag_obt<- dt_obt %>%
      summarise("SRAG (n)"=sum(SRAG),
                Positivos=sum(positivos),
                "Positivos (n)"=sum(positivos),
                "Flu A %"=round((sum(FLU_A)*100)/Positivos,1),
                "Flu_B %"=round((sum(FLU_B)*100)/Positivos,1),
                "Covid-19 %"=round(sum(SARS2)*100/Positivos),
                "VSR %"=round((sum(VSR)*100)/Positivos,1),
                "Rino %"=round((sum(RINO)*100)/Positivos,1),
                "Aden %"=round((sum(ADNO)*100)/Positivos,1),
                "Meta %"=round((sum(METAP)*100)/Positivos,1),
                "Boca %"=round((sum(BOCA)*100)/Positivos,1),
                "Paraflu %"=round((sum(PARA1, PARA2, PARA3, PARA4)*100)/Positivos,1),
                
      ) %>% mutate(Período="2024", .before = "SRAG (n)") %>%
      mutate(Dados="Óbitos", .before = "Período") %>%
      select(!Positivos)
    
    
    tabela_srag_4semanas_obt<- dt_obt %>%
      filter(epiweek>max_week) %>%
      summarise("SRAG (n)"=sum(SRAG),
                Positivos=sum(positivos),
                "Positivos (n)"=sum(positivos),
                "Flu A %"=round((sum(FLU_A)*100)/Positivos,1),
                "Flu_B %"=round((sum(FLU_B)*100)/Positivos,1),
                "Covid-19 %"=round(sum(SARS2)*100/Positivos),
                "VSR %"=round((sum(VSR)*100)/Positivos,1),
                "Rino %"=round((sum(RINO)*100)/Positivos,1),
                "Aden %"=round((sum(ADNO)*100)/Positivos,1),
                "Meta %"=round((sum(METAP)*100)/Positivos,1),
                "Boca %"=round((sum(BOCA)*100)/Positivos,1),
                "Paraflu %"=round((sum(PARA1, PARA2, PARA3, PARA4)*100)/Positivos,1),
                
      ) %>% mutate(Período="4 semanas", .before = "SRAG (n)") %>%
      mutate(Dados="Óbitos", .before = "Período") %>%
      select(!Positivos)
    
    
    
    junt<-bind_rows(tabela_srag, tabela_srag_4semanas, tabela_srag_obt, tabela_srag_4semanas_obt)
    
   # teste<- junt %>%
   #   mutate_at(.vars = 5:13, funs(paste0(. , "%", sep="")))
    
    DT::datatable(junt)
    
  })
  
  output$tabela_resumo <- DT::renderDataTable({
    tabela_download()
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
