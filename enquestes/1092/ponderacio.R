library(haven)
library(tidyverse)
library(readr)
library(survey)
library(weights)


## Carreguem les dades de l'enquesta ----------------------------

dades <- CEOdata::CEOdata(reo = "1092", raw = TRUE)
glimpse(dades)

dades_pond <- dades


## Carreguem les dades poblacionals ----------------------------
## En aquest estudi hem agrupat els trams d'estudis de forma diferent, així que el fitxer de dades poblacionals s'ha modificat.
poblacio_dist <- read_delim(file.path("poblacio.csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)
glimpse(poblacio_dist)


# Ens quedem amb les dades que tenim de l'últim any per fer les ponderacions
poblacio_dist <- poblacio_dist |>
  dplyr::select(identificador, codi_resposta, esp_18, any) |>
  group_by(identificador, codi_resposta) |>
  filter(any == max(any)) |>
  ungroup()


## Fixar semilla  ---------------------------------------
set.seed(3456)


## Variables per a fer les ponderacions ---------------------------------------
### Sexe i edat (SEXE, EDAT_GR)--------------------------------

table(dades_pond$SEXE, dades_pond$EDAT_GR, useNA = "always") / nrow(dades_pond)

dades_pond <- dades_pond |> 
  group_by(SEXE, EDAT_GR) |> 
  mutate(
    SEXE_EDAT_SVY = case_when(EDAT_GR != 99 & SEXE == 1 ~ paste0(SEXE, EDAT_GR),
                              EDAT_GR != 99 & SEXE == 2 ~ paste0("6", EDAT_GR),
                              TRUE ~ "99")
  ) |> 
  ungroup()

table(dades_pond$SEXE_EDAT_SVY, useNA = "always")

sexe_edat_dist <- as.data.frame(
  poblacio_dist |> 
    filter(identificador == "SEXE_EDAT_SVY") |>
    select(codi_resposta, esp_18)  |> 
    rename(SEXE_EDAT_SVY = codi_resposta) |>
    mutate( Freq = ((esp_18 / 100) * nrow(dades_pond) ) ) |> 
    select(SEXE_EDAT_SVY, Freq) )


### Grandària de municipi --------------------------------
# Atenció, agrupat en 5 categories

table(dades_pond$HABITAT, useNA = "always")

dades_pond <- dades_pond |> 
  mutate(HABITAT_SVY = case_when(
    HABITAT < 3 ~ 1,
    HABITAT ==3~2,
    HABITAT ==4~3,
    HABITAT ==5~4,
    HABITAT ==6~5))

table(dades_pond$HABITAT_SVY, useNA = "always")

habitat_dist <- as.data.frame(
  poblacio_dist |> 
    filter(identificador == "HABITAT") |>
    select(codi_resposta, esp_18) |> 
    mutate( Freq = (esp_18 / 100) * 1 * nrow(dades_pond) ) |> 
    select(codi_resposta, Freq) |>
    rename(HABITAT_SVY = codi_resposta))


### Nivell d'estudis (ESTUDIS_2) --------------------------------
# Atenció, agrupat en 2 nivells d'estudis

table(dades_pond$ESTUDIS_1_6, useNA = "always")

dades_pond <- dades_pond |> 
mutate( ESTUDIS_SVY = case_when(
  ESTUDIS_1_6 < 3 ~ 1,
  ESTUDIS_1_6 >= 3 & ESTUDIS_1_6 < 5 ~ 2,
  TRUE ~ 3))

table(dades_pond$ESTUDIS_SVY, useNA = "always")

estudis_dist <- as.data.frame(
  poblacio_dist |> 
    filter(identificador == "ESTUDIS_2") |>
    select(codi_resposta, esp_18) |> 
    mutate( Freq = (esp_18 / 100) * 1 * nrow(dades_pond) ) |> 
    select(codi_resposta, Freq) |>
    rename(ESTUDIS_SVY = codi_resposta))


### Lloc de naixement (LLOC_NAIX) --------------------------------
table(dades_pond$LLOC_NAIX, useNA = "always")

dades_pond <- dades_pond |> 
  mutate( LLOC_NAIX_SVY = case_when(
    LLOC_NAIX == 1 ~ 1,
    LLOC_NAIX == 2 ~ 2,
    TRUE ~ 5))

table(dades_pond$LLOC_NAIX_SVY, useNA = "always")

naix_dist <- as.data.frame(
  poblacio_dist |> 
    filter(identificador == "LLOC_NAIX") |>
    select(codi_resposta, esp_18) |> 
    mutate( Freq = (esp_18 / 100) * 1 * nrow(dades_pond) ) |> 
    select(codi_resposta, Freq) |>
    rename(LLOC_NAIX_SVY = codi_resposta))


### Llengua primera (LLENGUA_PRIMERA) --------------------------------
table(dades_pond$LLENGUA_PRIMERA_1_3, useNA = "always")

dades_pond <- dades_pond |> 
  mutate( LLENGUA_PRIMERA_SVY = case_when(
    LLENGUA_PRIMERA_1_3 == 1 ~ 1,
    LLENGUA_PRIMERA_1_3 == 2 ~ 2,
    TRUE ~ 80))

table(dades_pond$LLENGUA_PRIMERA_SVY, useNA = "always")

llengua_dist <- as.data.frame(
  poblacio_dist %>% 
    filter(identificador == "LLENGUA_PRIMERA")) %>% 
  select(codi_resposta, esp_18) %>% 
  mutate( Freq = (esp_18 / 100) * 1 * nrow(dades_pond)) %>% 
  select(codi_resposta, Freq) %>% 
  rename(LLENGUA_PRIMERA_SVY = codi_resposta)


### Província --------------------------------
table(dades_pond$PROVINCIA, useNA = "always")

dades_pond <- dades_pond |> 
  mutate( PROVINCIA_SVY = case_when(
    PROVINCIA == 8 ~ 8,
    PROVINCIA == 17 ~ 17,
    PROVINCIA == 25 ~ 25,
    PROVINCIA == 43 ~ 43))

table(dades_pond$PROVINCIA_SVY, useNA = "always")

provincia_dist <- as.data.frame(
  poblacio_dist %>% 
    filter(identificador == "PROVINCIA")) %>% 
  select(codi_resposta, esp_18) %>% 
  mutate( Freq = (esp_18 / 100) * 1 * nrow(dades_pond)) %>% 
  select(codi_resposta, Freq) %>% 
  rename(PROVINCIA_SVY = codi_resposta)


## Raking  --------------------------------------

# Creem les dades d'enquestes sense ponderació i assignant un mostreig aleatori simple
dades_svy_unweighted <- svydesign(ids = ~ 1, data = dades_pond)

# Ponderem 
sample_margins <- list(~SEXE_EDAT_SVY, ~HABITAT_SVY, ~ESTUDIS_SVY, ~LLOC_NAIX_SVY, ~LLENGUA_PRIMERA_SVY, ~PROVINCIA_SVY )
population_margins <- list(sexe_edat_dist, habitat_dist, estudis_dist, naix_dist, llengua_dist, provincia_dist )

dades_svy_rake <- rake(design = dades_svy_unweighted,
                       sample.margins = sample_margins,
                       population.margins = population_margins)




## Eliminació de pesos extrems (trimming) ----------------------------------------

dades_svy_rake_trim10 <- trimWeights(dades_svy_rake,
                                     lower = 0.1,
                                     upper = 10,
                                     strict = TRUE) 


summary(weights(dades_svy_rake))
summary(weights(dades_svy_rake_trim10))



## Creem nova variable en les dades definitives ----------------------------------------
dades_pond$PONDERA <- weights(dades_svy_rake_trim10)
glimpse(dades_pond)


## Revisem la ponderació

#Sexe
taula_sexe_pond <- dades_pond %>% 
  count(SEXE, wt = PONDERA) %>%
  mutate(prop_pond = (n / sum(n))*100)

taula_sexe <- dades_pond %>% 
  count(SEXE) %>% 
  mutate(proportion = (n / sum(n))*100)

t_sexe<-taula_sexe %>% 
  cbind(taula_sexe_pond)
t_sexe

#Edat
taula_edat_pond <- dades_pond %>% 
  count(EDAT_GR, wt = PONDERA)%>% 
  mutate(prop_pond = (n / sum(n))*100)

taula_edat <- dades_pond %>% 
  count(EDAT_GR)%>% 
  mutate(prop_pond = (n / sum(n))*100)

t_edat<-taula_edat %>% 
  cbind(taula_edat_pond)
t_edat

#Habitat
taula_habitat_pond <- dades_pond %>% 
  count(HABITAT_SVY, wt = PONDERA)%>% 
  mutate(prop_pond = (n / sum(n))*100)

taula_habitat <- dades_pond %>% 
  count(HABITAT_SVY)%>% 
  mutate(prop_pond = (n / sum(n))*100)

t_habitat<-taula_habitat %>% 
  cbind(taula_habitat_pond)
t_habitat


#Estudis
taula_estudis_pond <- dades_pond %>% 
  count(ESTUDIS_SVY, wt = PONDERA)%>% 
  mutate(prop_pond = (n / sum(n))*100)

taula_estudis <- dades_pond %>% 
  count(ESTUDIS_SVY)%>% 
  mutate(prop_pond = (n / sum(n))*100)

t_estudis<-taula_estudis %>% 
  cbind(taula_estudis_pond)
t_estudis


#Lloc_naix
taula_lloc_naix_pond <- dades_pond %>% 
  count(LLOC_NAIX_SVY, wt = PONDERA)%>% 
  mutate(prop_pond = (n / sum(n))*100)

taula_lloc_naix <- dades_pond %>% 
  count(LLOC_NAIX_SVY)%>% 
  mutate(prop_pond = (n / sum(n))*100)

t_lloc_naix<-taula_lloc_naix %>% 
  cbind(taula_lloc_naix_pond)
t_lloc_naix

#Llengua primera
taula_llengua_primera_pond <- dades_pond %>% 
  count(LLENGUA_PRIMERA_SVY, wt = PONDERA)%>% 
  mutate(prop_pond = (n / sum(n))*100)

taula_llengua_primera <- dades_pond %>% 
  count(LLENGUA_PRIMERA_SVY)%>% 
  mutate(prop_pond = (n / sum(n))*100)

t_llengua_primera<-taula_llengua_primera %>% 
  cbind(taula_llengua_primera_pond)
t_llengua_primera

#provincia
taula_prov_pond <- dades_pond %>% 
  count(PROVINCIA, wt = PONDERA)%>% 
  mutate(prop_pond = (n / sum(n))*100)
taula_prov_pond

#Exportar les dades a .sav
#install.packages("rio")
library(rio)
export(dades_pond,"Microdades_ponderacio.sav")
