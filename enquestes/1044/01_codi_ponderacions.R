library(haven)
library(tidyverse)
library(readr)
library(CEOdata)
library(survey)
library(weights)

## Carreguem les dades de l'enquesta ----------------------------
dades <- CEOdata(reo = "1044")

dades_pond <- dades |>
  filter(MODE_ADMIN == 1) |>
  dplyr::select(ORDRE, SEXE, EDAT_GR, LLOC_NAIX, LLENGUA_PRIMERA, ESTUDIS_1_6, CLUSTER21)


## Carreguem les dades poblacionals ----------------------------
poblacio_dist <- read_delim(file.path("dta", "poblacio.csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)

# Ens quedem amb les dades que tenim de l'últim any per fer les ponderacions
poblacio_dist <- poblacio_dist |>
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


sexe_edat_dist <- as.data.frame(
  poblacio_dist |> 
    filter(identificador == "SEXE_EDAT_SVY") |>
    select(codi_resposta, res_16)  |> 
    rename(SEXE_EDAT_SVY = codi_resposta) |>
    mutate( Freq = ((res_16 / 100) * nrow(dades_pond) ) ) |> 
    select(SEXE_EDAT_SVY, Freq) )


### Lloc de naixement (LLOC_NAIX)--------------------------------
table(dades_pond$LLOC_NAIX, useNA = "always")

lloc_naix_dist <- as.data.frame(
  poblacio_dist |> 
    filter(identificador == "LLOC_NAIX") |>
    select(codi_resposta, res_16)  |> 
    mutate( Freq = ((res_16 / 100) * nrow(dades_pond) ) ) |> 
    select(codi_resposta, Freq) |>
    rename(LLOC_NAIX = codi_resposta) )


### Llengua primera (LLENGUA_PRIMERA)--------------------------------
table(dades_pond$LLENGUA_PRIMERA, useNA = "always")

llengua_primera_dist <- as.data.frame(
  poblacio_dist |> 
    filter(identificador == "LLENGUA_PRIMERA") |>
    select(codi_resposta, res_16)  |> 
    # La categoria altres no existeix en la població, anem a crear una categoria fictícia
    # on la seva proporció a la població sigiui gairebé 0.
    bind_rows(data.frame( codi_resposta = 99, res_16 = 0.1/(10^100) )) |> 
    mutate( Freq = ((res_16 / 100) * nrow(dades_pond) ) ) |> 
    select(codi_resposta, Freq) |>
    rename(LLENGUA_PRIMERA = codi_resposta) )


### Nivell d'estudis (ESTUDIS_1_6) --------------------------------
table(dades_pond$ESTUDIS_1_6, useNA = "always")

dades_pond <- dades_pond |> 
  mutate( ESTUDIS_SVY = case_when( ESTUDIS_1_6 <= 1 ~ 1,
                                   ESTUDIS_1_6 <= 4 ~ 2,
                                   ESTUDIS_1_6 <= 6 ~ 3,
                                   TRUE ~ 4) )

table(dades_pond$ESTUDIS_SVY, useNA = "always")

estudis_dist <- as.data.frame(
  poblacio_dist |> 
    filter(identificador == "ESTUDIS") |>
    select(codi_resposta, res_16) |> 
    # La categoria altres no existeix en la població, anem a crear una categoria fictícia
    # on la seva proporció a la població sigiui gairebé 0.
    bind_rows(data.frame( codi_resposta = 4, res_16 = 0.1/(10^100) )) |> 
    mutate( Freq = (res_16 / 100) * 1 * nrow(dades_pond) ) |> 
    select(codi_resposta, Freq) |>
    rename(ESTUDIS_SVY = codi_resposta)  )



### Tipologia de seccio censal (CLUSTER21) --------------------------------
table(dades_pond$CLUSTER21, useNA = "always")

cluster_dist <- as.data.frame(
  poblacio_dist |>
    filter(identificador == "CLUSTER21",
           codi_resposta <= 6) |>
    select(codi_resposta, res_16) |>
    mutate(Freq = (res_16 / 100) * 1 * nrow(dades_pond)) |>
    select(codi_resposta, Freq) |>
    rename(CLUSTER21 = codi_resposta))

poblacio_dist |>
  filter(identificador == "CLUSTER21",
         codi_resposta <= 6) 



## Raking  --------------------------------------

# Creem les dades d'enquestes sense ponderació i assignant un mostreig aleatori simple
dades_svy_unweighted <- svydesign(ids = ~ 1, data = dades_pond)

# Ponderem 
sample_margins <- list(~SEXE_EDAT_SVY, ~LLOC_NAIX,  ~LLENGUA_PRIMERA, ~ESTUDIS_SVY, ~CLUSTER21)
population_margins <- list(sexe_edat_dist, lloc_naix_dist, llengua_primera_dist, estudis_dist, cluster_dist)

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
dades_pond$PONDERA_ONLINE <- weights(dades_svy_rake_trim10)

