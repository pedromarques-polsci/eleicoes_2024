# ATENCAO
# ESTE SCRIPT FOI FEITO ESPECIFICAMENTE PARA UM ARTIGO DO JC
# COMO O TSE NAO LIBEROU DADOS DO SEGUNDO TURNO AINDA, EU ADICIONEI OS RESULTADOS
# DO PL E DO PT
# OS DADOS AQUI SERVEM PARA ANALISAR APENAS O DESEMPENHO DO PT E DO PL, POR 
# ENQUANTO

# PACOTES -----------------------------------------------------------------
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(electionsBR) == F) 
  install.packages('electionsBR'); require(electionsBR)
if(require(geobr) == F) install.packages('geobr'); require(geobr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(janitor) == F) install.packages('janitor'); require(janitor)
if(require(purrr) == F) install.packages('purrr'); require(purrr)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(zoo) == F) install.packages('zoo'); require(zoo)

rm(list = ls())

# POPULACAO ---------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveProject()))

sidra_estpop <-read_rds("processed_data/sidra_estpop.rds") %>% 
  mutate(tcode = as.character(tcode))

censo_pop <- read.csv2("raw_data/estimativa_pop/estimativa_2000_2010.csv",
                       header = 4, skip = 3,nrows = 5566) %>% 
  slice(-1) %>% clean_names() %>% rename('2000' = x2000, 
                                         '2010' = x2010) %>% 
  pivot_longer(cols = c('2000','2010'), values_to = 'estpop', 
               names_to = 'ano') %>% 
  mutate(ano = as.numeric(ano)) %>% 
  rename(tcode = cod)

estpop2023 <- read_xls("raw_data/estimativa_pop/estimativa_2023.xls", 
                       range = "A2:E5572") %>% 
  clean_names() %>% 
  mutate(tcode = paste0(cod_uf, cod_munic),
         ano = 2023) %>% 
  rename(estpop = populacao,
         municipio = nome_do_municipio)

estpop2024 <- read_xls("raw_data/estimativa_pop/estimativa_2024.xls", 
                       sheet = 2,
                       range = "A2:E5572") %>% 
  clean_names() %>% 
  mutate(tcode = paste0(cod_uf, cod_munic),
         ano = 2024) %>% 
  rename(estpop = populacao_estimada,
         municipio = nome_do_municipio)

populacao <- rbind(censo_pop %>% select(ano, tcode, municipio, estpop),
                   sidra_estpop %>% select(ano, tcode, municipio, estpop),
                   estpop2023 %>% select(ano, tcode, municipio, estpop),
                   estpop2024 %>% select(ano, tcode, municipio, estpop)) %>% 
  arrange(ano, tcode) %>% 
  filter(estpop != "...") %>% 
  group_by(tcode) %>%
  mutate(estpop = na.approx(estpop, na.rm = F)) %>% 
  ungroup()

write_rds(populacao, "processed_data/populacao.rds")

# ELEITOS -----------------------------------------------------------------

## EXTRACAO -------------------------------------------------------------------
get_may <- function(x){
  elections_tse(year = x, type = "vote_mun_zone", 
                br_archive = TRUE) %>% 
    clean_names() %>% 
    filter(cd_sit_tot_turno == 1) %>% 
    distinct(cd_municipio, .keep_all = T) %>% 
    select(ano_eleicao, sg_uf, cd_municipio, nm_municipio,
           sg_partido, nr_partido, nm_partido, 
           nm_coligacao, ds_composicao_coligacao, 
           nm_urna_candidato) %>% 
    mutate(cd_municipio = as.numeric(cd_municipio))
}

prefeitos <- purrr::map(c(2000, 2004, 2008, 2012, 2016, 2020),
                              ~get_may(.x)) %>%
                          purrr::list_rbind()

write_rds(prefeitos, "processed_data/prefeito_2000_2020.rds")

## ENRIQUECIMENTO ------------------------------------------------------------
prefeitos <- read_rds("processed_data/prefeito_2000_2020.rds")
resultados_2024 <- read.csv2("raw_data/2024_primeiro_turno.csv",
                                 fileEncoding = "latin1") %>% 
  clean_names()

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 260001926362] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 110002118922] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 60002129518] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 90002213855] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 250001921615] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 250002140911] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 250001971533] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$cd_sit_tot_turno == 6] <- 4

resultados_2024 <- resultados_2024 %>% 
  filter(cd_sit_tot_turno == 1) %>% 
  distinct(cd_municipio, .keep_all = T) %>% 
  select(ano_eleicao, sg_uf, cd_municipio, nm_municipio,
         sg_partido, nr_partido, nm_partido, 
         nm_coligacao, ds_composicao_coligacao, 
         nm_urna_candidato) %>% 
  mutate(cd_municipio = as.numeric(cd_municipio))

tse_ibge <- read_rds("processed_data/tse_ibge.rds") %>% 
  mutate(city_tse = as.numeric(city_tse))

populacao <- read_rds("processed_data/populacao.rds")

final_dataset_prefeito <- rbind(prefeitos, resultados_2024) %>% 
  left_join(tse_ibge, join_by(cd_municipio == city_tse)) %>% 
  left_join(populacao %>% select(-municipio) %>% 
              mutate(tcode = as.numeric(tcode)), 
            join_by(city_ibge == tcode, ano_eleicao == ano))

# Adicionei dados do segundo turno referentes ao PL e ao PT
write_rds(final_dataset_prefeito, 
          "processed_data/prefeito_2000_2024_provisorio.rds")

# NUMERO PREFEITURAS ----------------------------------------------------------
final_dataset_prefeito <- 
  read_rds("processed_data/prefeito_2000_2024_provisorio.rds") %>% 
  mutate(pop_int = cut(estpop,
                       breaks = c(0, 10000, 50000, 190000,
                                    Inf),
                       labels = c('[0-10]', '(10-50]', 
                                  '(50-190]', '(190+]'
                                  )
                       )
         ) %>% 
  mutate(regiao = case_when(
    sg_uf %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ 
      "Nordeste",
    sg_uf %in% c("GO", "MT", "MS", "DF") ~ "Centro-Oeste",
    sg_uf %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
    sg_uf %in% c("ES", "MG", "SP", "RJ") ~ "Sudeste",
    sg_uf %in% c("PR", "SC", "RS") ~ "Sul",
  )) %>% 
  mutate(estpop_m = estpop/1000)

geral <- final_dataset_prefeito %>% 
  group_by(ano_eleicao, sg_partido) %>% 
  summarise(n = n()) %>% 
  group_by(ano_eleicao) %>% 
  mutate(freq = n / sum(n) * 100)

por_uf <- final_dataset_prefeito %>% 
  group_by(ano_eleicao, sg_uf, sg_partido) %>% 
  summarise(n = n()) %>% 
  group_by(ano_eleicao, sg_uf) %>% 
  mutate(freq = n / sum(n) * 100)

por_regiao <- final_dataset_prefeito %>% 
  group_by(ano_eleicao, regiao, sg_partido) %>% 
  summarise(n = n()) %>% 
  group_by(ano_eleicao, regiao) %>% 
  mutate(freq = n / sum(n) * 100)

por_regiao_res <- final_dataset_prefeito %>% 
  group_by(ano_eleicao, regiao, sg_partido) %>% 
  summarise(n = n(),
            pop = sum(estpop, na.rm = T)) %>% 
  group_by(ano_eleicao, regiao) %>% 
  mutate(freq = n / sum(n) * 100,
         freq_pop = pop / sum(pop))

geral_res <- final_dataset_prefeito %>% 
  group_by(ano_eleicao, sg_partido) %>% 
  summarise(n = n(),
            pop = sum(estpop, na.rm = T)) %>% 
  group_by(ano_eleicao) %>% 
  mutate(freq = n / sum(n) * 100,
         freq_pop = pop / sum(pop))

geral %>% filter(sg_partido == "PL")

geral %>% filter(sg_partido == "PT")

por_regiao %>% filter(sg_partido == "PL") %>% View()

por_regiao_res %>% filter(sg_partido == "PT") %>% View()

por_regiao_res %>% filter(sg_partido == "PL") %>% View()

geral_res %>% filter(sg_partido == "PL") %>% View()

por_regiao_pop %>% filter(sg_partido == "PT") %>% View()

por_regiao_pop %>% filter(sg_partido == "PL") %>% View()

diferenca <- final_dataset_prefeito %>% 
  group_by(regiao, ano_eleicao, pop_int, sg_partido) %>% 
  summarise(qtd = n()) %>% 
  ungroup() %>% 
  filter(!is.na(pop_int)) %>% 
  complete(regiao, ano_eleicao, pop_int, sg_partido) %>% 
  mutate(qtd = coalesce(qtd,0)) %>% 
  group_by(regiao, sg_partido, pop_int) %>% 
  summarise(diff = qtd[which(ano_eleicao==2024)] - qtd[which(ano_eleicao==2020)])
  
graph_pl <- diferenca %>% filter(sg_partido == "PL") %>% 
  ggplot(aes(x = regiao, y = diff, fill = pop_int)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("Região") +
  ylab("Prefeituras") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(fill='População (milhares)', 
       title = "Figura 1. Variação no número de prefeituras do PL (2020-2024)",
       caption = "Fonte: dados do TSE") +
  scale_fill_manual(values = c("pink", "magenta2", "orange", "red"))

graph_pl

ggsave('plot/graph_pl.jpeg', dpi = 500, height = 5, width = 10,
       unit = 'in', graph_pl)

graph_pt <- diferenca %>% filter(sg_partido == "PT") %>% 
  ggplot(aes(x = regiao, y = diff, fill = pop_int)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("Região") +
  ylab("Prefeituras") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(fill='População (milhares)', 
       title = "Figura 2. Variação no número de prefeituras do PT (2020-2024)",
       caption = "Fonte: dados do TSE") +
  scale_fill_manual(values = c("pink", "magenta2", "orange", "red"))

graph_pt

ggsave('plot/graph_pt.jpeg', dpi = 500, height = 5, width = 10,
       unit = 'in', graph_pt) 

final_dataset_prefeito %>% filter(ano_eleicao == 2024, sg_partido == "PL") %>% 
  ggplot() +
  geom_bar(mapping = aes(x = pop_int, fill = regiao)) +
  xlab("Intervalo populacional") +
  ylab("Prefeituras") +
  ggtitle("Número de prefeituras conquistadas pelo PL (2024)")

final_dataset_prefeito %>% filter(ano_eleicao == 2020, sg_partido == "PL") %>% 
  ggplot() +
  geom_bar(mapping = aes(x = pop_int, fill = regiao)) +
  xlab("Intervalo populacional") +
  ylab("Prefeituras") +
  ggtitle("Número de prefeituras conquistadas pelo PL (2020)")

final_dataset_prefeito %>% filter(ano_eleicao == 2024, sg_partido == "PT") %>% 
  ggplot() +
  geom_bar(mapping = aes(x = regiao, fill = pop_int)) +
  xlab("Intervalo populacional") +
  ylab("Prefeituras") +
  ggtitle("Número de prefeituras conquistadas pelo PT (2024)")

final_dataset_prefeito %>% filter(ano_eleicao == 2020, sg_partido == "PT") %>% 
  ggplot() +
  geom_bar(mapping = aes(x = regiao, fill = pop_int)) +
  xlab("Intervalo populacional") +
  ylab("Prefeituras") +
  ggtitle("Número de prefeituras conquistadas pelo PT (2020)")

final_dataset_prefeito %>% group_by(regiao) %>% 
  mutate()

final_dataset_prefeito %>% filter(sg_partido == "PL",
                                  regiao == "NE") %>% 
  mutate()
  ggplot() +
  geom_bar(mapping = aes(x = pop_int)) +
  xlab("Intervalo populacional") +
  ylab("Prefeituras") +
  ggtitle("Número de prefeituras conquistadas pelo PL (2024)")

final_dataset_prefeito %>% filter(ano_eleicao == 2020, sg_partido == "PL",
                                  regiao == "NE") %>% 
  ggplot() +
  geom_bar(mapping = aes(x = pop_int)) +
  xlab("Intervalo populacional") +
  ylab("Prefeituras") +
  ggtitle("Número de prefeituras conquistadas pelo PL (2020)")

# NUMERO CANDIDATOS -------------------------------------------------------
resultados_2024 <- read.csv2("raw_data/2024_primeiro_turno.csv",
                             fileEncoding = "latin1") %>% 
  clean_names()

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 260001926362] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 110002118922] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 60002129518] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 90002213855] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 250001921615] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 250002140911] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$sq_candidato == 250001971533] <- 1

resultados_2024$cd_sit_tot_turno[
  resultados_2024$cd_sit_tot_turno == 6] <- 4

resultados_2024 <- resultados_2024 %>% 
  filter(cd_cargo == 11) %>% 
  distinct(sq_candidato, .keep_all = T) %>% 
  select(ano_eleicao, sg_uf, cd_municipio, nm_municipio,
         sq_candidato, nm_urna_candidato, 
         sg_partido, nr_partido, nm_partido, 
         nm_coligacao, ds_composicao_coligacao,
         cd_sit_tot_turno, ds_sit_tot_turno
         ) %>% 
  mutate(cd_municipio = as.numeric(cd_municipio))

candidatura <- resultados_2024 %>% 
  group_by(cd_municipio) %>% 
  mutate(pl_cand = ifelse(sg_partido == "PL", 1, 0),
         pl_vit = ifelse(pl_cand == 1 & cd_sit_tot_turno == 1, 1, 0),
         pt_cand = ifelse(sg_partido == "PT", 1, 0),
         pl_colig = ifelse(sg_partido != "PL" & 
                             grepl(ds_composicao_coligacao, pattern = "PL"),
                           1, 0),
         pt_vit = ifelse(pt_cand == 1 & cd_sit_tot_turno == 1, 1, 0)) %>% 
  mutate(regiao = case_when(
    sg_uf %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "NE",
    sg_uf %in% c("GO", "MT", "MS", "DF") ~ "CO",
    sg_uf %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "NO",
    sg_uf %in% c("ES", "MG", "SP", "RJ") ~ "SE",
    sg_uf %in% c("PR", "SC", "RS") ~ "SUL"))

voto_2022 <-read.csv2("votacao_candidato_munzona_2022_BR.csv") %>% 
  clean_names() %>% 
  filter(cd_cargo == 1, nr_turno ==2) %>% 
  group_by(cd_municipio) %>% 
  mutate(voto_total = sum(qt_votos_nominais_validos),
         percent_pt = 
           sum(qt_votos_nominais_validos[which(sg_partido == "PT")])/
           voto_total * 100,
         percent_pl = 
           sum(qt_votos_nominais_validos[which(sg_partido == "PL")])/
           voto_total * 100) %>% 
  distinct(cd_municipio, .keep_all = T) %>% 
  select(cd_municipio, nm_municipio, voto_total, percent_pt, percent_pl)

dataset <- candidatura %>% 
  left_join(voto_2022 %>% select(cd_municipio,percent_pt,percent_pl), 
            join_by(cd_municipio)) %>% 
  distinct(cd_municipio, .keep_all = T) %>% 
  select(sg_uf, cd_municipio, nm_municipio, pl_cand, pl_vit, 
         pt_cand, pt_vit, percent_pt, percent_pl, pl_colig) %>% 
  mutate(regiao = case_when(
    sg_uf %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "NE",
    sg_uf %in% c("GO", "MT", "MS", "DF") ~ "CO",
    sg_uf %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "NO",
    sg_uf %in% c("ES", "MG", "SP", "RJ") ~ "SE",
    sg_uf %in% c("PR", "SC", "RS") ~ "SUL")) %>% 
  group_by(regiao) %>% 
  mutate(pl_vit_tx = sum(pl_vit)/sum(pl_cand) * 100,
    pt_vit_tx = sum(pt_vit)/sum(pt_cand)  * 100,
    pl_cand_tx = sum(pl_cand)/n() * 100,
    pt_cand_tx = sum(pt_cand)/n() * 100,
    pl_vit_qt = sum(pl_vit),
    pt_vit_qt = sum(pt_vit),
    pl_cand_qt = sum(pl_cand),
    pt_cand_qt = sum(pt_cand),
    pl_colig_qt = sum(pl_colig)
    ) %>% 
  ungroup()

dataset %>% 
  distinct(regiao, .keep_all = T) %>% select(regiao:pl_colig_qt) %>% View()

dataset_ne <- dataset %>% filter(regiao == "NE")

dataset_not_ne <- dataset %>% filter(regiao != "NE")

m1 <- glm(pl_vit ~ percent_pl, family = "binomial", dataset)

m1 %>% summary()

exp(coef(m1))

m2 <- glm(pl_cand ~ percent_pl, family = "binomial", dataset)

m2 %>% summary()

exp(coef(m2))

mpt1<- glm(pt_cand ~ percent_pt, family = "binomial", dataset)

mpt1 %>% summary()

exp(coef(mpt1))

mpt2<- glm(pt_vit ~ percent_pt, family = "binomial", dataset)

mpt2 %>% summary()

exp(coef(mpt2))