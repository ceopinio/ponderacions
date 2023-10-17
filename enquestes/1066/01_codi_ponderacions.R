library(haven)
library(tidyverse)
library(readr)
library(CEOdata)
library(survey)
library(weights)
library(pewmethods)

## Carreguem les dades de l'enquesta ----------------------------
dades <- CEOdata(reo = "1066", raw = T)

## Selecció de variables necessaries per a les ponderacions ----------------------------
dades_raking <- dades |>
  filter(MODE_ADMIN == 1) |>
  dplyr::select(IDENTIFICADOR, MODE_ADMIN, SEXE, EDAT_GR, LLOC_NAIX, LLENGUA_PRIMERA_1_3, ESTUDIS_1_6, CLUSTER21, HABITAT, PROVINCIA)

## Carreguem les dades poblacionals ----------------------------
poblacio_dist <- read_delim(file.path("dta", "poblacio.csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)

## Carreguem funcions que es necessiten ----------------------------
source("R/obtenir_dist.R")

## Neteja dades poblacionals ----------------------------
# Ens quedem amb les dades poblacionals mes recents
poblacio_dist <- poblacio_dist |>
  group_by(identificador, codi_resposta) |>
  filter(any == max(any)) |>
  ungroup() |>
  # Indiquem les dades poblacionals de referència
  rename(valor = res_16, # Opcions: res_16, res_18, esp_15, esp_18
         resposta = resposta_16) |> # Opcions: resposta_16, resposta_18
  select(identificador_svy, codi_resposta, valor, resposta) |>
  rename(identificador = identificador_svy)


# Tractament de les categories de la mostra no presents a la població ----------------------------

# Abans de calcular les ponderacions, es mira si les categories que tenen les variables coincideixen amb les que hi ha a la població.
# Pels casos en què la categoria que hi ha a la mostra no hi és en la població, es posa com un valor faltant (`NA`).
# 
# - Nivell d'estudis (ESTUDIS_1_6)
# - Lloc naixement (LLOC_NAIX)
# - Llengua primera (LLENGUA_PRIMERA)
# - Estudis (ESTUDIS_1_6)
# - Cluster (CLUSTER21): similitud electoral


dades_raking <- dades_raking |> 
    ### SEXE i EDAT_GR -------------------------------------------------
  group_by(SEXE, EDAT_GR) |> 
    mutate(
      SEXE_EDAT_SVY = case_when(EDAT_GR > 0 & EDAT_GR != 99 & SEXE == 1 ~ paste0(SEXE, EDAT_GR),
                                EDAT_GR > 0 & EDAT_GR != 99 & SEXE == 2 ~ paste0("6", EDAT_GR),
                                TRUE ~ NA_character_),
      SEXE_EDAT_SVY = as.factor(SEXE_EDAT_SVY)
    ) |> 
    ungroup() |>
    ### LLOC_NAIX -------------------------------------------------
  mutate(
    LLOC_NAIX_SVY = as.factor( ifelse(LLOC_NAIX %in% c(1, 2, 5), LLOC_NAIX, NA) )
  ) |>
    ### LLENGUA_PRIMERA -------------------------------------------------
  mutate(
    LLENGUA_PRIMERA_SVY = as.factor( ifelse(LLENGUA_PRIMERA_1_3 %in% c(1, 2, 80), LLENGUA_PRIMERA_1_3, NA) )
  ) |>
    ### ESTUDIS -------------------------------------------------
  mutate(
    ESTUDIS_SVY = case_when( ESTUDIS_1_6 < 0 ~ NA_real_,
                             ESTUDIS_1_6 == 1 ~ 1,
                             ESTUDIS_1_6 <= 4 ~ 2,
                             ESTUDIS_1_6 <= 6 ~ 3,
                             TRUE ~ NA_real_),
    ESTUDIS_SVY = as.factor(ESTUDIS_SVY),
  ) |>
    ### CLUSTER21 -------------------------------------------------
  mutate(
    CLUSTER21_SVY = as.factor( ifelse(CLUSTER21 %in% c(1:6), CLUSTER21, NA) )
  )


# Random Forest imputation ----------------------------

# En algunes variables que s'usen per la ponderació hi ha valors faltants, per tant, es du a terme una imputació d'aquests. 
# Per a fer-ho s'usa la funció `impute_vars` del paquet [pewmethods](https://github.com/pewresearch/pewmethods), que es basa en una imputació 
# Random Forest a partir de les variables que tenen més relació amb les que tenen valors faltants.

colSums(is.na(dades_raking))

dades_imputed <- dades_raking %>% 
  select(-IDENTIFICADOR, -MODE_ADMIN) %>%
  impute_vars(to_impute = c("SEXE_EDAT_SVY", "LLOC_NAIX_SVY", "LLENGUA_PRIMERA_SVY", "ESTUDIS_SVY", "CLUSTER21_SVY",
                            "HABITAT", "PROVINCIA"),
              seed = 739)

colSums(is.na(dades_imputed))



# Obtenció de les dades poblacionals ----------------------------

# A partir de les dades poblacionals, es calcula la freqüència poblacional a partir dels individus de la mostra. 
# Aquest càlcul es fa per a totes les variables que s'escullen per a fer la ponderació, ja que l'algoritme que s'usa 
# més endavant necessita aquesta informació.

### SEXE i EDAT_GR -------------------------------------------------
sexe_edat_dist <- obtenir_dist("SEXE_EDAT_SVY", dades_imputed, poblacio_dist)

### LLOC_NAIX -------------------------------------------------
lloc_naix_dist <- obtenir_dist("LLOC_NAIX_SVY", dades_imputed, poblacio_dist)

### LLENGUA_PRIMERA -------------------------------------------------
llengua_primera_dist <- obtenir_dist("LLENGUA_PRIMERA_SVY", dades_imputed, poblacio_dist)

### ESTUDIS -------------------------------------------------
estudis_dist <- obtenir_dist("ESTUDIS_SVY", dades_imputed, poblacio_dist)

### CLUSTER21 -------------------------------------------------
cluster_dist <- obtenir_dist("CLUSTER21_SVY", dades_imputed, poblacio_dist)



# Raking ----------------------------

# Creem les dades d'enquestes sense ponderació i assignant un mostreig aleatori simple
dades_svy_unweighted <- svydesign(ids = ~ 1, data = dades_imputed)

# Ponderem 
sample_margins <- list(~SEXE_EDAT_SVY, ~LLOC_NAIX_SVY,  ~LLENGUA_PRIMERA_SVY, ~ESTUDIS_SVY, ~CLUSTER21_SVY)
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


# Diagnosi ----------------------------------------
# Obtenir l'aproximació de Kish
# Obtenim l'efecte del disseny, efecte de la mida de la mostra i l'error d'estimació
calculate_deff(weights(dades_svy_rake_trim10))



## Creem nova variable en les dades definitives ----------------------------------------
dades_raking$PONDERA <- weights(dades_svy_rake_trim10)

