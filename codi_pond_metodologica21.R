#Ponderacions Enquesta metodològica i actituds sociopolítiques
#Centre d'estudis d'opinió

library(haven)
library(tidyverse)
library(readr)

library(survey)

#setwd("~/OneDrive - Generalitat de Catalunya/Documents/Dades x anàlisi CEO/Enquesta metodològica")
#setwd("~/Dades x anàlisi CEO/Enquesta metodològica")
dades <- read_sav("W:/Projectes/1. Estudis en curs/1. Propis/P-2021-27. Enquesta metodològica i actituds/Resultats/Base de dades/Microdades revisades.sav")


#Recodifiquem variables i introduim les dades de població

#Lloc de naixement
table(dades$LLOC_NAIX)
dades$naixement<-dades$LLOC_NAIX
dades$naixement[dades$LLOC_NAIX==4]<-3
table(dades$naixement)

naixement.real <- data.frame(naixement = c("1", "2", "3"),
                             Freq = nrow(dades) * c(0.698,	0.224, 0.078))

#Sexe i edat
dades<-dades %>% group_by(EDAT_GR, SEXE_PANEL) %>% mutate(sexe_edat = cur_group_id()) %>% ungroup()
table(dades$sexe_edat)
prop.table(table(dades$sexe_edat))

sexe.edat.real <- data.frame(sexe_edat = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                             Freq = nrow(dades) * c(0.0540,	0.0510, 0.0577,	0.0559, 0.1353,	0.1341, 0.1240,	0.1293, 0.1111,	0.1478))


#Llengua inicial
table(dades$LLENGUA_PRIMERA)
dades$llenguaini<-dades$LLENGUA_PRIMERA
dades$llenguaini[dades$LLENGUA_PRIMERA==99]<-98
dades$llenguaini[dades$LLENGUA_PRIMERA==4]<-80

table(dades$llenguaini)
llenguaini.real <- data.frame(llenguaini = c("1", "2", "3", "80", "98"),
                              Freq = nrow(dades) * c(0.357,0.562,0.032,0.037,0.011))

#Educació
table(dades$EDUCACION_PANEL)
dades$edu<-dades$EDUCACION_PANEL
edu.real <- data.frame(edu = c("1","2", "3", "4"),
                       Freq = nrow(dades) * c(0.162,	0.278,	0.234,	0.325))

#Mida de municipi

table(dades$HABITAT)
dades$tamuni<-dades$HABITAT
dades$tamuni<-as.numeric(dades$tamuni)
tamuni.real<- data.frame(tamuni = c("1","2", "3", "4", "5", "6"),
                         Freq = nrow(dades) * c(0.04779, 0.14399, 0.27778, 0.20469, 0.11943, 0.20631))

##RAKING

dades.svy.unweighted <- svydesign(ids=~1, data=dades)


dades.svy.rake <- rake(design = dades.svy.unweighted,
                       sample.margins = list(~sexe_edat, ~llenguaini, ~edu, ~tamuni, ~naixement),
                       population.margins = list(sexe.edat.real, llenguaini.real, edu.real, tamuni.real, naixement.real))

#Resum

summary(weights(dades.svy.rake))

#Retallem els pesos amb la funció trimWeights.

dades.svy.rake.trim10 <- trimWeights(dades.svy.rake, lower=0.1, upper=10,
                                     strict=TRUE) 

summary(weights(dades.svy.rake.trim10))


dades$PONDERA2<-weights(dades.svy.rake.trim10)

