# Load necessary library
library(shiny)

# Define the UI for the application
ui <- fluidPage(
  # Add custom CSS to ensure images do not exceed the window size
  tags$style(HTML("
        .responsive-img {
            max-width: 100%;
            max-height: 500px;  /* Restrict maximum height */
            width: auto;
            height: auto;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }
        .logo-banner {
            max-width: 100px; /* Adjust the size as needed */
            display: block;
            margin-left: auto;
            margin-right: auto;
        }
        .title-panel {
            text-align: center;
            font-size: 24px;
            font-weight: bold;
            padding-left: 20px; /* Spacing between logo and title */
            display: inline-block;
        }
        .figure-title {
            text-align: center; /* Ensure titles are centered */
            margin-left: 15px;  /* Consistent left margin for titles */    
            margin-top: 40px;    /* Add some margin above the titles */
        }
        .figure-space {
            margin-bottom: 60px; /* Add space between figures */
        }
    ")),
  
  # Include the logo and title in the same row
  fluidRow(
    column(1, img(src = "https://yourdomain.com/logo.png", class = "logo-banner")),  # Replace with your logo URL
    column(11, div("Dashboard de resultados do boletim InfoGripe", class = "title-panel"))  # Title in the second column
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
               column(6, uiOutput("brazil_figure1")),
               column(6, uiOutput("brazil_figure2"))
             ),
             
             # Figure 3 alone with a header
             h3("Figura 2. Incidência e mortalidade nas últimas 8 semanas.", 
                class = "figure-title"),
             fluidRow(
               column(12, uiOutput("brazil_figure3"))
             ),
             
             # Figures 4 and 5 side by side with a header
             h3("Figura 3. Média da incidência e mortalidade semanal de SRAG nas últimas oito semanas, por vírus e faixa etária de interesse.",
                class = "figure-title"),
             fluidRow(
               column(6, uiOutput("brazil_figure4")),
               column(6, uiOutput("brazil_figure5"))
             ),
             
             # Figure 6 alone at the bottom
             h3("Figura 4. Incidência semanal de SRAG e por vírus identificado laboratorialmente, por faixas etárias de interesse.", 
                class = "figure-title"),
             fluidRow(
               column(12, uiOutput("brazil_figure6"))
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
               column(6, uiOutput("state_image1")),
               column(6, uiOutput("state_image2"))
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
               column(6, uiOutput("capital_image1")),
               column(6, uiOutput("capital_image2"))
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  ### Tab 1: Trends Figures ###
  output$trends_figure1 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/Mapa_ufs_tendencia.png", class = "responsive-img", alt = "Trends Figure 1")
  })
  
  output$trends_figure2 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/Capitais/Mapa_capitais_tendencia.png", class = "responsive-img", alt = "Trends Figure 2")
  })
  
  ### Tab 2: Brazil Static Figures ###
  output$brazil_figure1 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR.png", class = "responsive-img", alt = "Figure 1 for Brazil")
  })
  
  output$brazil_figure2 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR_fx_etaria.png", class = "responsive-img", alt = "Figure 2 for Brazil")
  })
  
  output$brazil_figure3 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR_inc_mort.png", class = "responsive-img", alt = "Figure 3 for Brazil")
  })
  
  output$brazil_figure4 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR_virus_lab_inc.png", class = "responsive-img", alt = "Figure 4 for Brazil")
  })
  
  output$brazil_figure5 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR_virus_lab_mort.png", class = "responsive-img", alt = "Figure 5 for Brazil")
  })
  
  output$brazil_figure6 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/fig_BR_virus_lab.png", class = "responsive-img", alt = "Figure 6 for Brazil")
  })
  
  ### Tab 3: Brazilian States ###
  get_image_url <- function(state, image_number) {
    base_url <- "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/"  # Replace with your base URL
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
    tags$img(src = get_image_url(state, 1), class = "responsive-img", alt = paste("State", state, "Image 1"))
  })
  
  output$state_image2 <- renderUI({
    state <- input$selected_state
    tags$img(src = get_image_url(state, 2), class = "responsive-img", alt = paste("State", state, "Image 2"))
  })
  
  output$state_image3 <- renderUI({
    state <- input$selected_state
    tags$img(src = get_image_url(state, 3), class = "responsive-img", alt = paste("State", state, "Image 3"))
  })
  
  ### Tab 4: Brazilian Capitals ###
  get_capital_image_url <- function(capital, image_number) {
    # Map capital names to state abbreviations
    state_abbr <- switch(capital,
                         "Rio Branco" = "AC", "Maceió" = "AL", "Macapá" = "AP", "Manaus" = "AM",
                         "Salvador" = "BA", "Fortaleza" = "CE", "Brasília" = "DF", "Vitória" = "ES",
                         "Goiânia" = "GO", "São Luís" = "MA", "Cuiabá" = "MT", "Campo Grande" = "MS",
                         "Belo Horizonte" = "MG", "Belém" = "PA", "João Pessoa" = "PB", "Curitiba" = "PR",
                         "Recife" = "PE", "Teresina" = "PI", "Rio de Janeiro" = "RJ", "Natal" = "RN",
                         "Porto Alegre" = "RS", "Porto Velho" = "RO", "Boa Vista" = "RR", "Florianópolis" = "SC",
                         "São Paulo" = "SP", "Aracaju" = "SE", "Palmas" = "TO")
    
    base_url <- "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/"  # Replace with your base URL
    capital_formatted <- gsub(" ", "_", toupper(capital))
    
    if (image_number == 1) {
      return(paste0(base_url, "fig_", state_abbr, "_", capital_formatted, ".png"))
    } else if (image_number == 2) {
      return(paste0(base_url, "fig_", state_abbr, "_", capital_formatted, "_fx_etaria.png"))
    }
  }
  
  output$capital_image1 <- renderUI({
    capital <- input$selected_capital
    tags$img(src = get_capital_image_url(capital, 1), class = "responsive-img", alt = paste("Capital", capital, "Image 1"))
  })
  
  output$capital_image2 <- renderUI({
    capital <- input$selected_capital
    tags$img(src = get_capital_image_url(capital, 2), class = "responsive-img", alt = paste("Capital", capital, "Image 2"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
