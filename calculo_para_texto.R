library(rio)
library(lubridate)

path_tendencia = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/estados_e_pais_serie_estimativas_tendencia_sem_filtro_febre.csv"
dados_tendencia <- import(path_tendencia, dec = ",")

df_uf_crescimento <- dados_tendencia |> 
  janitor::clean_names() |> 
  mutate(
    casos_estimados = as.numeric(casos_estimados)
  ) |> 
  filter(
    ds_uf_sigla != "BR",
    ano_epidemiologico == year(Sys.Date()),
    semana_epidemiologica == isoweek(Sys.Date()) - 1,
    escala == "incidência",
    # tendencia_de_longo_prazo >= 0.5
    ) |> 
  select(
    casos_estimados, tendencia_de_longo_prazo, co_uf, ds_uf_sigla, semana_epidemiologica, ano_epidemiologico, escala
  )

df_uf_crescimento |> 
  arrange(ds_uf_sigla) |> 
  pull(ds_uf_sigla) |> 
  knitr::combine_words(and = " e ")

path_limiares = "https://gitlab.fiocruz.br/lsbastos/infogripe_code/-/raw/main/MEM_SRAG/output/limiares_UF_capitais.csv"

dados_limiares <- read.csv(path_limiares, dec = ",") |> 
  janitor::clean_names()

dados_limiares |> 
  filter(escala == "incidencia") |> 
  mutate(
    across(c(muito_baixo:muito_alto), ~ as.numeric(.))
  ) |> 
  pivot_longer(
    cols = c("muito_baixo":"muito_alto")) |> 
  group_by(regiao) |> 
  mutate(
    names2 = paste0(
      seq(1, 5),
      " - ",
      name
  )
) |> 
  left_join(
    df_uf_crescimento,
    by = c("cod_regiao" = "co_uf")) |> 
  mutate(
    teste = (casos_estimados > value)
  ) |> 
  filter(teste) |> 
  slice_tail() |> 
  filter(
    name == "moderado" | name == "alto" | name == "muito_alto"
  )

df_uf_crescimento
