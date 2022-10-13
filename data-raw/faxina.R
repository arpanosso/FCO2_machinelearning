# entrada de dados
library(tidyverse)
library(janitor)
library(readxl)
source("R/minhas-funcoes.R")

data_set <- read_xlsx("data-raw/Dados_Doc.xlsx") %>%
  clean_names() %>%
  mutate(
    oco2 = ifelse(oco2 == 9999, NA, oco2),
    f_771 = ifelse(f_771 == 9999, NA, f_771*2.57743*10^(-19)),
    f_757 = ifelse(f_757 == 9999, NA, f_757*2.6250912*10^(-19)),
    sif = (f_757 + 1.5*f_771)/2
  ) %>%
  select(-(f_771:f_757))
glimpse(data_set)

data_set %>%
  group_by(experimento, data, tratamento, cultura) %>%
  mutate(
     fco2_exp = resumo_espacial(experimento,fco2),
     us_exp = resumo_espacial(experimento,us),
     ts_exp = resumo_espacial(experimento,ts),
     clay_exp = resumo_espacial(experimento,clay),
     trat_cult = interaction(tratamento, cultura,sep = '-')
   ) %>%
  ggplot(aes(x=clay, y=fco2, color=as_factor(trat_cult))) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm")

## motivação: será cultura e tratamento similares na informação carregada?
  data_set %>% pull(tratamento) %>% unique()
  data_set %>% pull(cultura) %>% unique()


data_set <- data_set %>%
    group_by(experimento, data, tratamento, cultura) %>%
    mutate(
      fco2_exp = resumo_espacial(experimento,fco2),
      us_exp = resumo_espacial(experimento,us),
      ts_exp = resumo_espacial(experimento,ts),
      clay_exp = resumo_espacial(experimento,clay),
      trat_cult = interaction(tratamento, cultura,sep = '-')
    ) %>% ungroup()

write_rds(data_set,"data/dados_todos.rds")
