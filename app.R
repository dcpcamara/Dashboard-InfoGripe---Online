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
    ),
    tabPanel("Sobre o InfoGripe",
             fluidRow(
               column(12, 
                      div(style = "margin: 20px; font-size: 16px; line-height: 1.5;",
                          HTML("<p><strong>InfoGripe</strong> é uma iniciativa para monitorar e apresentar níveis de alerta para os casos reportados de Síndrome Respiratória Aguda Grave (SRAG) no SINAN, o Sistema de Informação de Agravos de Notificação (<a href='http://www.saude.gov.br/sinan' target='_blank'>www.saude.gov.br/sinan</a>). Os dados são apresentados por estado e por regiões de vigilância para síndromes gripais.</p>
                          <p>Este é um produto da parceria entre pesquisadores do Programa de Computação Científica da Fundação Oswaldo Cruz (Fiocruz, PROCC), da Escola de Matemática Aplicada da Fundação Getúlio Vargas (FGV, EMAp), no Rio de Janeiro, e do extinto GT-Influenza e atual Coordenação-Geral de Vigilância das Síndromes Gripais da Secretaria de Vigilância em Saúde do Ministério da Saúde (CGGRIPE, SVS, MS). O InfoGripe integra o conjunto de estudos que estão sendo mapeados com a  <a href='https://lac.tghn.org/proyectos-pathfinder/pathfinder-brasil/infogripe/' target='_blank'>metodologia Pathfinder</a> pela The Global Health Network Latin America and the Caribbean (TGHN LAC)
                          <p><strong>Dados abertos e boletins:</strong></p>
                          <ul>
                            <li>Dados processados: <a href='https://bit.ly/infogripe-dados-fiocruz' target='_blank'>https://bit.ly/infogripe-dados-fiocruz</a>.</li>
                            <li>Boletins do InfoGripe: <a href='http://bit.ly/mave-infogripe-fiocruz' target='_blank'>http://bit.ly/mave-infogripe-fiocruz</a>.</li>
                          </ul>
                          <p><strong>Software livre:</strong> GNU General Public License v3</p>
                          <p><strong>Documentação:</strong> <a href='https://fludashboard.readthedocs.io/' target='_blank'>https://fludashboard.readthedocs.io/</a>.</p>
                          <p><strong>Publicações científicas:</strong></p>
                          <p>O modelo estatístico utilizado para estimativa de casos recentes (\"nowcasting\"), fundamental para reporte de situação oportuna e de qualidade, é baseado no trabalho desenvolvido em colaboração entre pesquisadores da Fiocruz e University of Exeter, UK: Bastos L, Economou T, Gomes M, Villela D, Coelho F, Cruz O, Stoner O, Bailey T, Codeço C. (2019). A modelling approach for correcting reporting delays in disease surveillance data, <em>Statistics in Medicine</em>, DOI: <a href='https://doi.org/10.1002/sim.8303' target='_blank'>10.1002/sim.8303</a>.</p>
                          <p><strong>Membros:</strong></p>
                          <ul>
                            <li><a href='http://lattes.cnpq.br/1929576902623348' target='_blank'>Claudia Torres Codeço</a> - PROCC-Fiocruz</li>
                            <li><a href='http://lattes.cnpq.br/4016632420686251' target='_blank'>Daniel Antunes Maciel Villela</a> - PROCC-Fiocruz</li>
                            <li><a href='http://lattes.cnpq.br/1360297898724057' target='_blank'>Daniel Cardoso Portela Câmara</a> - PROCC-Fiocruz</li>
                            <li><a href='http://lattes.cnpq.br/0309050626285266' target='_blank'>Flávio Codeço Coelho</a> - EMAp-FGV</li>
                            <li><a href='http://lattes.cnpq.br/5241799121437269' target='_blank'>Leonardo Soares Bastos</a> - PROCC-Fiocruz</li>
                            <li><a href='http://lattes.cnpq.br/2996805485281003' target='_blank'>Laís Picinini Freitas</a> - PROCC-Fiocruz</li>
                            <li><a href='http://lattes.cnpq.br/7282202947621572' target='_blank'>Luiz Max Fagundes de Carvalho</a> - EMAp-FGV</li>
                            <li><a href='http://lattes.cnpq.br/6064559192125515' target='_blank'>Marcelo Ferreira da Costa Gomes</a> - PROCC-Fiocruz</li>
                            <li><a href='http://lattes.cnpq.br/9530671289607786' target='_blank'>Oswaldo Gonçalves Cruz</a> - PROCC-Fiocruz</li>
                            <li><a href='http://lattes.cnpq.br/2518752229392005' target='_blank'>Raquel Martins Lana</a> - BSC, Barcelona, Espanha</li>
                            <li><a href='http://lattes.cnpq.br/8988655613888832' target='_blank'>Tatiana Pineda Portella Zenker</a> - PROCC-Fiocruz</li>
                          </ul>"
                          )
                      )
               )
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
    
    
    dt_srag <- dados %>% filter(DS_UF_SIGLA==input$selected_state) %>%
      filter(epiyear==2025, fx_etaria==input$selected_age)
    
    dt_obt <- dados_obt %>% filter(DS_UF_SIGLA==input$selected_state) %>%
      filter(epiyear==2025, fx_etaria==input$selected_age)
    
    # max_week<-max(ano$epiweek)-4
    
    tabela_srag<- dt_srag %>%
      filter(epiyear=="2025") %>%
      summarise("SRAG (n)"=sum(SRAG, na.rm = TRUE),
                Positivos=sum(positivos, na.rm = TRUE),
                "Positivos (n)"=sum(positivos, na.rm = TRUE),
                "Flu A %"=round((sum(FLU_A, na.rm = TRUE)*100)/Positivos,1),
                "Flu_B %"=round((sum(FLU_B, na.rm = TRUE)*100)/Positivos,1),
                "Covid-19 %"=round(sum(SARS2, na.rm = TRUE)*100/Positivos,1),
                "VSR %"=round((sum(VSR, na.rm = TRUE)*100)/Positivos,1),
                "Rino %"=round((sum(RINO, na.rm=TRUE)*100)/Positivos,1),
                "Aden %"=round((sum(ADNO, na.rm=TRUE)*100)/Positivos,1),
                "Meta %"=round((sum(METAP, na.rm=TRUE)*100)/Positivos,1),
                "Boca %"=round((sum(BOCA, na.rm=TRUE)*100)/Positivos,1),
                "Paraflu %"=round((sum(PARA1, PARA2, PARA3, PARA4, na.rm = TRUE)*100)/Positivos,1)) %>%
      mutate(Período="2025", .before = "SRAG (n)") %>%
      mutate(Dados="Casos", .before = "Período") %>%
      select(!Positivos)
    
    
    
    tabela_srag_4semanas<- dt_srag %>%
      #  filter(epiweek>max_week) %>%
      summarise("SRAG (n)"=sum(SRAG, na.rm = TRUE),
                Positivos=sum(positivos, na.rm = TRUE),
                "Positivos (n)"=sum(positivos, na.rm = TRUE),
                "Flu A %"=round((sum(FLU_A, na.rm = TRUE)*100)/Positivos,1),
                "Flu_B %"=round((sum(FLU_B, na.rm = TRUE)*100)/Positivos,1),
                "Covid-19 %"=round(sum(SARS2, na.rm = TRUE)*100/Positivos,1),
                "VSR %"=round((sum(VSR, na.rm = TRUE)*100)/Positivos,1),
                "Rino %"=round((sum(RINO, na.rm=TRUE)*100)/Positivos,1),
                "Aden %"=round((sum(ADNO, na.rm=TRUE)*100)/Positivos,1),
                "Meta %"=round((sum(METAP, na.rm=TRUE)*100)/Positivos,1),
                "Boca %"=round((sum(BOCA, na.rm=TRUE)*100)/Positivos,1),
                "Paraflu %"=round((sum(PARA1, PARA2, PARA3, PARA4, na.rm = TRUE)*100)/Positivos,1),
      ) %>% mutate(Período="4 semanas", .before = "SRAG (n)") %>%
      mutate(Dados="Casos", .before = "Período") %>%
      select(!Positivos)
    
    
    tabela_srag_obt<- dt_obt %>%
      filter(epiyear=="2025") %>%
      summarise("SRAG (n)"=sum(SRAG, na.rm = TRUE),
                Positivos=sum(positivos, na.rm = TRUE),
                "Positivos (n)"=sum(positivos, na.rm = TRUE),
                "Flu A %"=round((sum(FLU_A, na.rm = TRUE)*100)/Positivos,1),
                "Flu_B %"=round((sum(FLU_B, na.rm = TRUE)*100)/Positivos,1),
                "Covid-19 %"=round(sum(SARS2, na.rm = TRUE)*100/Positivos,1),
                "VSR %"=round((sum(VSR, na.rm = TRUE)*100)/Positivos,1),
                "Rino %"=round((sum(RINO, na.rm=TRUE)*100)/Positivos,1),
                "Aden %"=round((sum(ADNO, na.rm=TRUE)*100)/Positivos,1),
                "Meta %"=round((sum(METAP, na.rm=TRUE)*100)/Positivos,1),
                "Boca %"=round((sum(BOCA, na.rm=TRUE)*100)/Positivos,1),
                "Paraflu %"=round((sum(PARA1, PARA2, PARA3, PARA4, na.rm = TRUE)*100)/Positivos,1)) %>% 
      mutate(Período="2025", .before = "SRAG (n)") %>%
      mutate(Dados="Óbitos", .before = "Período") %>%
      select(!Positivos)
    
    
    tabela_srag_4semanas_obt<- dt_obt %>%
      #   filter(epiweek>max_week) %>%
      summarise("SRAG (n)"=sum(SRAG, na.rm = TRUE),
                Positivos=sum(positivos, na.rm = TRUE),
                "Positivos (n)"=sum(positivos, na.rm = TRUE),
                "Flu A %"=round((sum(FLU_A, na.rm = TRUE)*100)/Positivos,1),
                "Flu_B %"=round((sum(FLU_B, na.rm = TRUE)*100)/Positivos,1),
                "Covid-19 %"=round(sum(SARS2, na.rm = TRUE)*100/Positivos,1),
                "VSR %"=round((sum(VSR, na.rm = TRUE)*100)/Positivos,1),
                "Rino %"=round((sum(RINO, na.rm=TRUE)*100)/Positivos,1),
                "Aden %"=round((sum(ADNO, na.rm=TRUE)*100)/Positivos,1),
                "Meta %"=round((sum(METAP, na.rm=TRUE)*100)/Positivos,1),
                "Boca %"=round((sum(BOCA, na.rm=TRUE)*100)/Positivos,1),
                "Paraflu %"=round((sum(PARA1, PARA2, PARA3, PARA4, na.rm = TRUE)*100)/Positivos,1)) %>% 
      mutate(Período="4 semanas", .before = "SRAG (n)") %>%
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
