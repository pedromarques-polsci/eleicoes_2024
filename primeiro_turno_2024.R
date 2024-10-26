# PACOTES ----------------------------------------------------------------
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(geobr) == F) install.packages('geobr'); require(geobr)
if(require(ggplot) == F) install.packages('ggplot'); require(ggplot)
if(require(janitor) == F) install.packages('janitor'); require(janitor)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)

# ETL ---------------------------------------------------------------------
tse_ibge <- read_rds("processed_data/tse_ibge.rds")

first_round <- read.csv2("raw_data/2024_primeiro_turno.csv",
                         fileEncoding = "latin1") %>% 
  clean_names() %>% 
  mutate(sg_partido = case_when(sg_partido == "PC do B" ~ "PCDOB",
                           sg_partido == "CIDADANIA" ~ "CID",
                           sg_partido == "SOLIDARIEDADE" ~ "SD",
                           sg_partido == "REPUBLICANOS" ~ "REP",
                           TRUE ~ sg_partido),
         cd_municipio = str_pad(cd_municipio, width = 5, pad = "0"))

# 2. IDEOLOGIA (BOLOGNESI, RIBEIRO, CODATO, 2022) ---------------------

# Alguns partidos foram renomeados entre a aplicacao do survey e as eleicoes
# de 2020/2024
# PRB -> REPUBLICANOS (REP)
# PPS -> CIDADANIA (CID)
# PR -> PL
# PTC -> AGIR
# PMN -> MOBILIZA

# Alguns partidos foram fundidos
# UNIAO BRASIL = fusao entre DEM e PSL em 2021
# PRD = fusao entre Patriota e PTB

# Alguns partidos mudaram de nome antes da aplicação do survey
# PMDB -> MDB
# PTN -> Podemos
# PSDC -> DC
# PEN -> PATRI / Patriota (ha contreversias)
# PT do B -> Avante

# Gerando os dados de ideologia partidaria (p. 7-8, Tabela 1)
bolognesi.table <- data.frame(
  party = c("PSTU", "PCO", "PCB", "PSOL", "PCDOB", "PT", "PDT",
            "PSB", "REDE", "CID", "PPS", "PV", "PTB", 
            "AVANTE", "PT do B", "PT DO B",
            "SD", "PMN", "MOBILIZA", "PMB", "PHS", 
            "MDB", "PMDB", "PSD", "PSDB",
            "PODE", "PTN", "PPL", "PRTB", "PROS", 
            "PRP", "REP", "PRB", "PL", "PR",
            "PTC", "AGIR", "DC", "PSDC", "PSL", 
            "NOVO", "PP", "PSC", "PATRIOTA", "PEN", 
            "PATRI", "DEM", "UNIÃO", "PRD"),
  
  # Media dos posicionamentos
  ideo.bmean = c(0.51, 0.61, 0.91, 1.28, 1.92, 2.97, 3.92, # PSTU:PDT
                 4.05, 4.77, 4.92, 4.92, 5.29, 6.1, # PSB:PTB
                 6.32, 6.32, 6.32, # AVANTE:PT DO B
                 6.5, 6.88, 6.88, 6.9, 6.96, # SD:PHS
                 7.01, 7.01, 7.09, 7.11, # MSB:PSDB
                 7.24, 7.24, 7.27, 7.45, 7.47, # PODE:PROS
                 7.59, 7.78, 7.78, 7.78, 7.78, # PRP:PR
                 7.86, 7.86, 8.11, 8.11, 8.11, # PTC:PSL
                 8.13, 8.20, 8.33, 8.55, 8.55, # NOVO:PEN
                 8.55, 8.57, (8.57+8.11)/2, (8.55+6.1)/2)) # PATRI:PRD
    
# Reescalamento
bolognesi.table <- bolognesi.table %>% mutate(ideo.b = case_when(
    ideo.bmean <= 1.5 ~ -3,
    ideo.bmean >= 1.51 & ideo.bmean <= 3 ~ -2,
    ideo.bmean >= 3.01 & ideo.bmean <= 4.49 ~ -1,
    ideo.bmean >= 4.5 & ideo.bmean <= 5.5 ~ 0,
    ideo.bmean > 5.5 & ideo.bmean < 7.01 ~ 1,
    ideo.bmean > 7 & ideo.bmean <= 8.5 ~ 2,
    ideo.bmean >= 8.49 ~ 3))

## CAMARAS MUNICIPAIS -----------------------------------------------------
n_distinct(first_round$cd_municipio)
n_distinct(first_round$nm_municipio)

teste <- first_round %>% 
  distinct(cd_municipio)

teste_pe <- first_round %>% 
  filter(sg_uf == "PE") %>% 
  distinct(cd_municipio)

total_seat <- first_round %>% 
  filter(cd_cargo == 13, cd_sit_tot_turno %in% c(2:3)) %>% 
  group_by(sg_uf, cd_municipio) %>% 
  distinct(nr_candidato, .keep_all = T) %>% 
  reframe(seat = n(), nm_municipio = unique(nm_municipio))

setdiff(teste$cd_municipio, total_seat$cd_municipio)

seat_share <- first_round %>% 
  filter(cd_cargo == 13, cd_sit_tot_turno %in% c(2:3)) %>% 
  group_by(sg_uf, cd_municipio, sg_partido) %>% 
  distinct(nr_candidato, .keep_all = T) %>% 
  reframe(seat_share = n(), nm_municipio = unique(nm_municipio)) %>% 
  ungroup() %>% 
  left_join(total_seat %>% select(-nm_municipio), 
            join_by(sg_uf, cd_municipio)) %>% 
  left_join(bolognesi.table, join_by(sg_partido==party))

# Missings: PRD, AGIR, MOBILIZA

seat_share.b_mun <- seat_share %>% #filter(!is.na(leg_ideo.b)) %>% 
  group_by(sg_uf, cd_municipio) %>% 
  mutate(leg_party = paste(sg_partido, collapse = ", ")) %>% 
  reframe(leg_ideo.bmean = sum(seat_share * ideo.bmean / seat),
          leg_ideo.b = sum(seat_share * ideo.b / seat),
          sg_uf = unique(sg_uf),
          nm_municipio = unique(nm_municipio),
          leg_party = unique(leg_party),
          cd_municipio = unique(cd_municipio)) %>% 
  ungroup() %>% 
  left_join(tse_ibge, join_by(cd_municipio == city_tse))

## PREFEITURAS ----------------------------------------------------------
may_elect <- first_round %>% 
  filter(cd_cargo == 11, cd_sit_tot_turno == 1) %>% 
  group_by(sg_uf, cd_municipio) %>% 
  distinct(nr_candidato, .keep_all = T) %>% 
  select(sg_uf, cd_municipio, nm_municipio, nm_candidato, 
         sg_partido) %>% 
  left_join(bolognesi.table, join_by(sg_partido==party)) %>% 
  left_join(tse_ibge, join_by(cd_municipio == city_tse))

setdiff(teste$cd_municipio, may_elect$cd_municipio)

setdiff(teste_pe$cd_municipio, may_elect$cd_municipio)

may_pe <- ggplot() +
  labs(title = "Ideologia partidária dos prefeitos",
       subtitle = "Resultados do primeiro turno de 2024") +
  geom_sf(data = 
            read_municipality(code_muni = 26, year=2020) %>% 
            filter(code_muni != 2605459) %>% 
            left_join(may_elect, join_by(code_muni==city_ibge)), 
          aes(fill=ideo.bmean), size=.15) +
  scale_fill_gradientn(name = "Ideologia Partidária",
                       colors = c(low = "red", mid = "white", high = "blue"),
                       breaks = c(2, 8.5),
                       labels = c("Esquerda", "Direita"),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "white")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

may_pe

ggsave('plot/primeiro_turno_pe.jpeg', dpi = 300, height = 5, width = 10, 
       unit = 'in', may_pe)

legis_pe <- ggplot() +
  labs(title = "Ideologia partidária das câmaras municipais",
       subtitle = "Resultados do primeiro turno de 2024") +
  geom_sf(data = 
            read_municipality(code_muni = 26, year=2020) %>% 
            filter(code_muni != 2605459) %>% 
            left_join(seat_share.b_mun, join_by(code_muni==city_ibge)), 
          aes(fill=leg_ideo.bmean), size=.15) +
  scale_fill_gradientn(name = "Ideologia Partidária",
                       colors = c(low = "red", mid = "white", high = "blue"),
                       breaks = c(2, 8.5),
                       labels = c("Esquerda", "Direita"),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "white")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

legis_pe

# ESTATISTICAS DESCRITIVAS ------------------------------------------------
may_elect %>% summary(na.rm = T)

may_elect %>%filter(sg_uf == "PE") %>%  summary(na.rm = T)

may_elect %>% filter(sg_uf == "PE") %>% View()

may_elect %>% 
  filter(sg_uf == "PE") %>% 
  group_by(sg_partido) %>% 
  summarize(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(freq))

may_elect %>% 
  filter(sg_uf == "PE") %>% 
  count(as.factor(sg_partido)) %>% View()

seat_share.b_mun %>%filter(sg_uf == "PE") %>%  summary(na.rm = T)

seat_share.b_mun %>% filter(sg_uf == "PE") %>% View()