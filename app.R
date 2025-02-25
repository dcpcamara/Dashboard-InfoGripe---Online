# Load necessary library
library(shiny)
library(stringi)
library(tidyverse)
library(vroom)
library(janitor)
library(rio)
library(knitr)

####Baixando os dados

path = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv"
dados <- vroom(file = path)

path_obt = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/obitos_semanais_fx_etaria_virus_sem_filtro_febre.csv"
dados_obt <- vroom(file = path_obt)


###Baixando os dados

# Define the UI for the application
ui <- fluidPage(
  
  # Inserting Barra Brasil
  tags$div(
    id = "barra-brasil", 
    style = "background:#7F7F7F; height: 20px; padding:0 0 0 10px; display:block;",
    tags$ul(
      id = "menu-barra-temp",
      style = "list-style:none;",
      tags$li(
        style = "display:inline; float:left; padding-right:10px; margin-right:10px; border-right:1px solid #EDEDED;",
        tags$a(
          href = "http://brasil.gov.br",
          style = "font-family:sans,sans-serif; text-decoration:none; color:white;",
          "Portal do Governo Brasileiro"
        )
      )
    )
  ),
  
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
           tags$a(
             href = "http://info.gripe.fiocruz.br", target = "_blank",
             tags$img(src = "logo1.png", class = "logo-banner")
           )),
    column(1, 
           tags$a(
             href = "http://www.fiocruz.br/", target = "_blank",
             tags$img(src = "logo-marcafiocruz_vertical_POSITIVA_24052024.png", class = "logo-banner")
           )),
    column(1, 
           tags$img(src = "logo-sus.png", class = "logo-banner")
           ),
    column(1, 
           tags$a(
             href = "https://www.gov.br/saude/pt-br", target = "_blank",
             tags$img(src = "logo-MS.png", class = "logo-banner")
           )),
    column(1, 
           tags$a(
             href = "https://lac.tghn.org", target = "_blank",
             tags$img(src = "logo2.png", class = "logo-banner")
           )),
    column(10, 
           div("Dashboard de resultados do boletim InfoGripe", 
               class = "title-panel"))
  ),
  
  # Create a tab layout
  tabsetPanel(
    # First Tab: Trends
    tabPanel("Tendências",
             # Two figures vertically with titles
             h3("Mapa de atividade e tendência de SRAG - Unidades Federativas", 
                class = "figure-title"),
             fluidRow(
               uiOutput("trends_figure1")
             ),
             h3("Mapa de atividade e tendência de SRAG  - somente capitais", 
                class = "figure-title"),
             fluidRow(
               uiOutput("trends_figure2")
             )
    ),
    
    # Second Tab: Summary of the weekly results
    tabPanel("Resumo dos resultados",
             uiOutput("resumo_resultados")
    ),
    
    # Third Tab: Brazil
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
    
    # Fourth Tab: Brazilian States
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
    
    # Fifth Tab: Brazilian Capitals
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
    
    # Sixth Tab: Cases per virus
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
    
    # Seventh Tab: About InfoGripe
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
    ),
    tags$script(defer = "defer", src = "//barra.brasil.gov.br/barra_2.0.js", type = "text/javascript")
    
  )
)


# Define server logic
server <- function(input, output) {
  
  ### Tab 1: Trends Figures
  output$trends_figure1 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/UF/Mapa_ufs_intensidade_tendencia.png", 
             class = "responsive-img", 
             alt = "Trends Figure 1")
  })
  
  output$trends_figure2 <- renderUI({
    tags$img(src = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Boletins%20do%20InfoGripe/Imagens/Capitais/Mapa_capitais_intensidade_tendencia.png", 
             class = "responsive-img", 
             alt = "Trends Figure 2")
  })
  
  ### Tab 2: Summary of results
  ### Cases and deaths data
  # Cases
  dados_casos <- dados %>% 
    clean_names() %>% 
    filter(ds_uf_sigla == "BR",
           fx_etaria == "Total",
           ano_epidemiologico == year(Sys.Date()))
  
  df_casos <- dados_casos %>% 
    summarise(across(c(srag:aguardando), ~sum(.)))
  
  df_casos_4sem <- dados_casos %>% 
    filter(semana_epidemiologica >= isoweek(Sys.Date()) - 4,
           semana_epidemiologica <= isoweek(Sys.Date()) - 1) %>% 
    summarise(across(c(srag:aguardando), ~sum(.)))
  
  # Deaths
  dados_obitos <- dados_obt %>% 
    clean_names() %>% 
    filter(ds_uf_sigla == "BR",
           fx_etaria == "Total",
           ano_epidemiologico == year(Sys.Date()))
  
  df_obitos <- dados_obitos %>% 
    summarise(across(c(srag:aguardando), ~sum(.)))
  
  df_obitos_4sem <- dados_obitos %>% 
    filter(semana_epidemiologica >= isoweek(Sys.Date()) - 4,
           semana_epidemiologica <= isoweek(Sys.Date()) - 1) %>% 
    summarise(across(c(srag:aguardando), ~sum(.)))
  
  ### Trend data
  
  path_tendencia_uf <- "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/estados_e_pais_serie_estimativas_tendencia_sem_filtro_febre.csv"
  dados_tendencia_uf <- import(path_tendencia_uf, dec = ",") %>% 
    clean_names() %>% 
    mutate(casos_estimados = as.numeric(casos_estimados),
           tipo = "uf")
  
  path_tendencia_capitais <- "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/capitais_serie_estimativas_tendencia_sem_filtro_febre.csv"
  dados_tendencia_capitais <- import(path_tendencia_capitais, dec = ",") %>% 
    clean_names() %>% 
    mutate(casos_estimados = as.numeric(casos_estimados),
           tipo = "capital",
           co_mun_res_nome = str_to_title(co_mun_res_nome))
  
  df_uf_tendencia <- dados_tendencia_uf %>% 
    filter(ds_uf_sigla != "BR",
           ano_epidemiologico == year(Sys.Date()),
           semana_epidemiologica == isoweek(Sys.Date()) - 1,
           escala == "casos") %>% 
    select(casos_estimados, tendencia_de_longo_prazo, co_uf, ds_uf_sigla, tipo,
           semana_epidemiologica, ano_epidemiologico, escala) %>% 
    rename(regiao = ds_uf_sigla)
  
  df_capitais_tendencia <- dados_tendencia_capitais %>% 
    filter(ano_epidemiologico == year(Sys.Date()),
           semana_epidemiologica == isoweek(Sys.Date()) - 1,
           escala == "casos") %>% 
    select(casos_estimados, tendencia_de_longo_prazo, co_uf, co_mun_res_nome, tipo,
           semana_epidemiologica, ano_epidemiologico, escala) %>% 
    rename(regiao = co_mun_res_nome)
  
  df_tendencia <- bind_rows(df_uf_tendencia, df_capitais_tendencia)
  
  ### Activity level data
  
  path_atividade_uf <- "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/estados_intensidade_sem_filtro_febre.csv"
  dados_atividade_uf <- import(path_atividade_uf, dec = ",") %>% 
    clean_names() %>% 
    rename(regiao = ds_uf_sigla) %>% 
    mutate(tipo = "uf")
  
  path_atividade_capitais <- "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/capitais_intensidade_sem_filtro_febre.csv"
  dados_atividade_capitais <- import(path_atividade_capitais, dec = ",") %>% 
    clean_names() %>% 
    rename(regiao = co_mun_res_nome) %>% 
    mutate(tipo = "capital",
           regiao = str_to_title(regiao)) %>% 
    select(-co_mun_res, -ds_uf_sigla)
  
  df_atividade <- bind_rows(dados_atividade_uf, dados_atividade_capitais)
  
  ### UF full names
  
  df_ufs_extenso <- data.frame(
    uf_extenso = c("Acre", "Alagoas", "Amapá", "Amazonas",
                   "Bahia", "Ceará", "Distrito Federal", "Espírito Santo",
                   "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul",
                   "Minas Gerais", "Pará", "Paraíba", "Paraná",
                   "Pernambuco", "Piauí", "Rio de Janeiro", "Rio Grande do Norte",
                   "Rio Grande do Sul", "Rondônia", "Roraima", "Santa Catarina",
                   "São Paulo", "Sergipe", "Tocantins"),
    nm_uf = c("AC", "AL", "AP", "AM",
              "BA", "CE", "DF", "ES",
              "GO", "MA", "MT", "MS",
              "MG", "PA", "PB", "PR",
              "PE", "PI", "RJ", "RN",
              "RS", "RO", "RR", "SC",
              "SP", "SE", "TO"),
    stringsAsFactors = FALSE
  )
  
  # Joining activity level, trend and full names
  df_texto_filtro <- df_atividade %>% 
    select(-regiao) %>% 
    left_join(df_tendencia, by = c("co_uf", "tipo")) %>% 
    mutate(regiao = ifelse(regiao == "Regiao De Saude Central", "Brasília", regiao)) %>% 
    filter(intensidade %in% c("Alerta", "Risco", "Alto risco")) %>% 
    left_join(df_ufs_extenso, by = c("regiao" = "nm_uf"))
  
  df_ufs <- df_atividade %>% 
    filter(tipo == "uf") %>% 
    select(co_uf, regiao) %>% 
    rename(nm_uf = regiao)
  
  df_capitais_com_uf <- df_atividade %>% 
    select(-regiao) %>% 
    left_join(df_tendencia, by = c("co_uf", "tipo")) %>% 
    left_join(df_ufs) %>% 
    left_join(df_ufs_extenso) %>% 
    mutate(regiao = ifelse(regiao == "Regiao De Saude Central", "Brasília", regiao),
           capital_uf = ifelse(tipo == "capital", paste0(regiao, " (", nm_uf, ")"), NA_character_)) %>% 
    filter(intensidade %in% c("Alerta", "Risco", "Alto risco"))
  
  ### Creating the full text for the Result Summary tab
  output$resumo_resultados <- renderUI({
    
    ## Section "SARI Cases"
    # Total, positives and waiting
    casos_total_srag <- format(df_casos$srag, big.mark = ".", decimal.mark = ",")
    casos_total_positivos <- format(df_casos$positivos, big.mark = ".", decimal.mark = ",")
    casos_total_positivos_perc <- format(round(100 * df_casos$positivos / df_casos$srag, 1),
                                         big.mark = ".", decimal.mark = ",")
    casos_total_negativos <- format(df_casos$negativos, big.mark = ".", decimal.mark = ",")
    casos_total_negativos_perc <- format(round(100 * df_casos$negativos / df_casos$srag, 1),
                                         big.mark = ".", decimal.mark = ",")
    casos_total_aguardando <- format(df_casos$aguardando, big.mark = ".", decimal.mark = ",")
    casos_total_aguardando_perc <- format(round(100 * df_casos$aguardando / df_casos$srag, 1),
                                          big.mark = ".", decimal.mark = ",")
    
    # Year's totals
    casos_perc_flua <- format(round(100 * df_casos$flu_a / df_casos$positivos, 1),
                              big.mark = ".", decimal.mark = ",")
    casos_perc_flub <- format(round(100 * df_casos$flu_b / df_casos$positivos, 1),
                              big.mark = ".", decimal.mark = ",")
    casos_perc_vsr <- format(round(100 * df_casos$vsr / df_casos$positivos, 1),
                             big.mark = ".", decimal.mark = ",")
    casos_perc_rino <- format(round(100 * df_casos$rino / df_casos$positivos, 1),
                              big.mark = ".", decimal.mark = ",")
    casos_perc_sars2 <- format(round(100 * df_casos$sars2 / df_casos$positivos, 1),
                               big.mark = ".", decimal.mark = ",")
    
    # Last 4 weeks' totals
    casos_perc_4sem_flua <- format(round(100 * df_casos_4sem$flu_a / df_casos_4sem$positivos, 1),
                                   big.mark = ".", decimal.mark = ",")
    casos_perc_4sem_flub <- format(round(100 * df_casos_4sem$flu_b / df_casos_4sem$positivos, 1),
                                   big.mark = ".", decimal.mark = ",")
    casos_perc_4sem_vsr <- format(round(100 * df_casos_4sem$vsr / df_casos_4sem$positivos, 1),
                                  big.mark = ".", decimal.mark = ",")
    casos_perc_4sem_rino <- format(round(100 * df_casos_4sem$rino / df_casos_4sem$positivos, 1),
                                   big.mark = ".", decimal.mark = ",")
    casos_perc_4sem_sars2 <- format(round(100 * df_casos_4sem$sars2 / df_casos_4sem$positivos, 1),
                                    big.mark = ".", decimal.mark = ",")
    
    ## Section "SARI Deaths"
    # Total, positives and waiting
    obitos_total_srag <- format(df_obitos$srag, big.mark = ".", decimal.mark = ",")
    obitos_total_positivos <- format(df_obitos$positivos, big.mark = ".", decimal.mark = ",")
    obitos_total_positivos_perc <- format(round(100 * df_obitos$positivos / df_obitos$srag, 1),
                                          big.mark = ".", decimal.mark = ",")
    obitos_total_negativos <- format(df_obitos$negativos, big.mark = ".", decimal.mark = ",")
    obitos_total_negativos_perc <- format(round(100 * df_obitos$negativos / df_obitos$srag, 1),
                                          big.mark = ".", decimal.mark = ",")
    obitos_total_aguardando <- format(df_obitos$aguardando, big.mark = ".", decimal.mark = ",")
    obitos_total_aguardando_perc <- format(round(100 * df_obitos$aguardando / df_obitos$srag, 1),
                                           big.mark = ".", decimal.mark = ",")
    
    # Year's totals
    obitos_perc_flua <- format(round(100 * df_obitos$flu_a / df_obitos$positivos, 1),
                               big.mark = ".", decimal.mark = ",")
    obitos_perc_flub <- format(round(100 * df_obitos$flu_b / df_obitos$positivos, 1),
                               big.mark = ".", decimal.mark = ",")
    obitos_perc_vsr <- format(round(100 * df_obitos$vsr / df_obitos$positivos, 1),
                              big.mark = ".", decimal.mark = ",")
    obitos_perc_rino <- format(round(100 * df_obitos$rino / df_obitos$positivos, 1),
                               big.mark = ".", decimal.mark = ",")
    obitos_perc_sars2 <- format(round(100 * df_obitos$sars2 / df_obitos$positivos, 1),
                                big.mark = ".", decimal.mark = ",")
    
    # Last 4 weeks' totals
    obitos_perc_4sem_flua <- format(round(100 * df_obitos_4sem$flu_a / df_obitos_4sem$positivos, 1),
                                    big.mark = ".", decimal.mark = ",")
    obitos_perc_4sem_flub <- format(round(100 * df_obitos_4sem$flu_b / df_obitos_4sem$positivos, 1),
                                    big.mark = ".", decimal.mark = ",")
    obitos_perc_4sem_vsr <- format(round(100 * df_obitos_4sem$vsr / df_obitos_4sem$positivos, 1),
                                   big.mark = ".", decimal.mark = ",")
    obitos_perc_4sem_rino <- format(round(100 * df_obitos_4sem$rino / df_obitos_4sem$positivos, 1),
                                    big.mark = ".", decimal.mark = ",")
    obitos_perc_4sem_sars2 <- format(round(100 * df_obitos_4sem$sars2 / df_obitos_4sem$positivos, 1),
                                     big.mark = ".", decimal.mark = ",")
    
    ## Section "Situation in the states"
    num_ufs_afetadas_atividade <- nrow(df_texto_filtro %>% filter(tipo == "uf"))
    sem_epidemio_uf <- unique(df_texto_filtro %>% filter(tipo == "uf") %>% pull(semana_epidemiologica))
    ufs_afetadas_atividade <- df_texto_filtro %>% 
      filter(tipo == "uf") %>% 
      arrange(uf_extenso) %>% 
      pull(uf_extenso) %>% 
      combine_words(and = " e ", oxford_comma = FALSE)
    num_ufs_afetadas_tendencia <- nrow(df_texto_filtro %>% filter(tipo == "uf", tendencia_de_longo_prazo > 0.1))
    ufs_afetadas_tendencia <- df_texto_filtro %>% 
      filter(tipo == "uf", tendencia_de_longo_prazo > 0.1) %>% 
      arrange(uf_extenso) %>% 
      pull(uf_extenso) %>% 
      combine_words(and = " e ", oxford_comma = FALSE)
    
    ## Section "Situation in the capitals"
    num_capitais_afetadas_atividade <- nrow(df_texto_filtro %>% filter(tipo == "capital"))
    sem_epidemio_cap <- unique(df_texto_filtro %>% filter(tipo == "capital") %>% pull(semana_epidemiologica))
    capitais_afetadas_atividade <- df_capitais_com_uf %>% 
      filter(tipo == "capital") %>% 
      arrange(capital_uf) %>% 
      pull(capital_uf) %>% 
      combine_words(and = " e ", oxford_comma = FALSE)
    num_capitais_afetadas_tendencia <- nrow(df_texto_filtro %>% filter(tipo == "capital", tendencia_de_longo_prazo > 0.1))
    capitais_afetadas_tendencia <- df_capitais_com_uf %>% 
      filter(tipo == "capital", tendencia_de_longo_prazo > 0.1) %>% 
      arrange(capital_uf) %>% 
      pull(capital_uf) %>% 
      combine_words(and = " e ", oxford_comma = FALSE)
    
    ## Full HTML text
    texto_resumo <- paste0(
      "<div style='padding:20px; font-family: Arial, sans-serif; font-size:16px; line-height:1.5;'>",
      "<h3 style='color:#2C3E50; margin-bottom:20px;'>Casos de SRAG</h3>",
      "<p>Referente ao ano epidemiológico 2025, já foram notificados ", casos_total_srag, 
      " casos de SRAG, sendo ", casos_total_positivos, " (", casos_total_positivos_perc, 
      "%) com resultado laboratorial positivo para algum vírus respiratório, ", 
      casos_total_negativos, " (", casos_total_negativos_perc, 
      "%) negativos, e ao menos ", casos_total_aguardando, " (", casos_total_aguardando_perc, 
      "%) aguardando resultado laboratorial. Dados de positividade para semanas recentes estão sujeitos a grandes alterações em atualizações seguintes por conta do fluxo de notificação de casos e inserção do resultado laboratorial associado.</p>",
      "<p>Dentre os casos positivos do ano corrente, observou-se ", casos_perc_flua, 
      "% de Influenza A, ", casos_perc_flub, "% de Influenza B, ", casos_perc_vsr, 
      "% de vírus sincicial respiratório, ", casos_perc_rino, "% de Rinovírus, e ", 
      casos_perc_sars2, "% de SARS-CoV-2 (COVID-19). Nas 4 últimas semanas epidemiológicas, a prevalência entre os casos positivos foi de ", 
      casos_perc_4sem_flua, "% de Influenza A, ", casos_perc_4sem_flub, "% de Influenza B, ", 
      casos_perc_4sem_vsr, "% de vírus sincicial respiratório, ", casos_perc_4sem_rino, 
      "% de Rinovírus, e ", casos_perc_4sem_sars2, "% de SARS-CoV-2 (COVID-19).</p>",
      "<h3 style='color:#2C3E50; margin-bottom:20px;'>Óbitos de SRAG</h3>",
      "<p>Referente aos óbitos de SRAG em 2025, já foram registrados ", obitos_total_srag, 
      " óbitos de SRAG, sendo ", obitos_total_positivos, " (", obitos_total_positivos_perc, 
      "%) com resultado laboratorial positivo para algum vírus respiratório, ", 
      obitos_total_negativos, " (", obitos_total_negativos_perc, 
      "%) negativos, e ao menos ", obitos_total_aguardando, " (", obitos_total_aguardando_perc, 
      "%) aguardando resultado laboratorial.</p>",
      "<p>Dentre os óbitos positivos do ano corrente, observou-se ", obitos_perc_flua, 
      "% de Influenza A, ", obitos_perc_flub, "% de Influenza B, ", obitos_perc_vsr, 
      "% de vírus sincicial respiratório, ", obitos_perc_rino, "% de Rinovírus, e ", 
      obitos_perc_sars2, "% de SARS-CoV-2 (COVID-19). Nas 4 últimas semanas epidemiológicas, a prevalência entre os óbitos positivos foi de ", 
      obitos_perc_4sem_flua, "% de Influenza A, ", obitos_perc_4sem_flub, "% de Influenza B, ", 
      obitos_perc_4sem_vsr, "% de vírus sincicial respiratório, ", obitos_perc_4sem_rino, 
      "% de Rinovírus, e ", obitos_perc_4sem_sars2, "% de SARS-CoV-2 (COVID-19).</p>",
      "<h3 style='color:#2C3E50; margin-bottom:20px;'>Situação nos estados</h3>",
      "<p>Na presente atualização, observa-se que ", num_ufs_afetadas_atividade, 
      " das 27 unidades federativas apresentam nível de atividade de SRAG em alerta, risco ou alto risco (últimas duas semanas) até a semana ", 
      sem_epidemio_uf, ": ", ufs_afetadas_atividade, ". Dentre essas UFs, ", num_ufs_afetadas_tendencia, 
      " também apresentam sinal de crescimento de SRAG na tendência de longo prazo (últimas 6 semanas) até a semana ", 
      sem_epidemio_uf, ": ", ufs_afetadas_tendencia, ".</p>",
      "<h3 style='color:#2C3E50; margin-bottom:20px;'>Situação nas capitais</h3>",
      "<p>Na presente atualização, observa-se que ", num_capitais_afetadas_atividade, 
      " das 27 capitais apresentam nível de atividade de SRAG em alerta, risco ou alto risco (últimas duas semanas) até a semana ", 
      sem_epidemio_cap, ": ", capitais_afetadas_atividade, ". Dentre essas capitais, ", num_capitais_afetadas_tendencia, 
      " também apresentam sinal de crescimento de SRAG na tendência de longo prazo (últimas 6 semanas) até a semana ", 
      sem_epidemio_cap, ": ", capitais_afetadas_tendencia, ".</p>",
      "</div>"
    )
    
    HTML(texto_resumo)
    
  })
  
  ### Tab 3: Brazil Static Figures
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
  
  ### Tab 4: Brazilian States
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
  
  ### Tab 5: Brazilian Capitals
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
  
  ###Tab 6
  
  tabela_download <- reactive({
    
    dt_srag <- dados %>% filter(DS_UF_SIGLA==input$selected_state) %>%
      filter(epiyear==2025, fx_etaria==input$selected_age)
    
    dt_obt <- dados_obt %>% filter(DS_UF_SIGLA==input$selected_state) %>%
      filter(epiyear==2025, fx_etaria==input$selected_age)
    
    max_week<-max(dt_srag$epiweek)-4
    
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
      filter(epiweek>max_week) %>%
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
      filter(epiweek>max_week) %>%
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
