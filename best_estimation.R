rm(list=ls())

source("fun.R")
library(dplyr)
library(rlang)
library(sae)
library(survey)
library(nlme)
library(TeachingSampling)
library(stringr)
options(survey.lonely.psu="adjust")

#**********************************************************
## 1. Lectura de los datos ####
#**********************************************************

# Población
est <- readRDS("./data/estudiantes.rds")
sum(est$EVALUADOS)
nrow(est)

# Muestra
# primera etapa NII / nII
# segunda es NI / nI
# tercera es N_i / n_i
# La multiplicación son los factores de expasión
muestra3etapas <- readRDS("./data/muestra3etapas.RDS")
diseno_muestral <- svydesign(ids = ~CODIGOMUNICIPIO + CODIGO_ICFES + ID_estud,
                             strata = ~estrato_mpio + EstratoColegio,
                             fpc = ~ NI + NII + N_i, data = muestra3etapas,
                             nest = T)
sum(weights(diseno_muestral))


#************************************************
## 2. Definición del RETO ####
#************************************************
# Dominio: Depto (cod dpto)
# MODELAR con intercepto
# Y = puntaje matemáticas
# X1 = puntaje materias (1 sola)
# X2 = estrato servicios públicos
# X3 = Variable colegio (jornada, oficial/privado, calendario)
# X4 = ? una que quiera

# revisar video 8:30 pm aprox
# Usar función del paquete SAE para el diseño menstrual

#***************************************
## 3. Revisión de los datos ####
#***************************************
# Se imputan datos faltantes siguiendo las reglas a continuación:
str(est)

est$CODIGO_ICFES[is.na(est$CODIGO_ICFES)]
est$AGSB_NOMBREINSTITUCION[is.na(est$AGSB_NOMBREINSTITUCION)]
est$CODIGOMUNICIPIO[is.na(est$CODIGOMUNICIPIO)]
est$CALENDARIO[is.na(est$CALENDARIO)]
est$CALENDARIO <- ifelse(est$CALENDARIO=="Calendario A", "Calendario_A",
                         ifelse(est$CALENDARIO=="Calendario B", "Calendario_B",
                                ifelse(est$CALENDARIO=="Calendario flexible", "Calendario_flexible","Sin_Calendario")))
muestra3etapas$CALENDARIO <- ifelse(muestra3etapas$CALENDARIO=="Calendario A", "Calendario_A",
                                    ifelse(muestra3etapas$CALENDARIO=="Calendario B", "Calendario_B",
                                           ifelse(muestra3etapas$CALENDARIO=="Calendario flexible", 
                                                  "Calendario_flexible","Sin_Calendario")))

est$NATURALEZA[is.na(est$NATURALEZA)]
est$JORNADA[is.na(est$JORNADA)]
est$PERS_GENERO[is.na(est$PERS_GENERO)] <- names(table(est$PERS_GENERO)[which.max(table(est$PERS_GENERO))])
est$FINS_ESTRATOVIVIENDAENERGIA[is.na(est$FINS_ESTRATOVIVIENDAENERGIA)] <- "1"
est$FINS_PERSONASHOGARACTUAL[is.na(est$FINS_PERSONASHOGARACTUAL)] <- round(mean(est$FINS_PERSONASHOGARACTUAL, na.rm=T))
est$FINS_PERSONASHOGARACTUAL <- as.numeric(est$FINS_PERSONASHOGARACTUAL)
est$FINS_CUARTOSHOGARACTUAL[is.na(est$FINS_CUARTOSHOGARACTUAL)] <- round(mean(est$FINS_CUARTOSHOGARACTUAL, na.rm=T))
est$FINS_CUARTOSHOGARACTUAL <- as.numeric(est$FINS_CUARTOSHOGARACTUAL)
est$FINS_PISOSHOGAR[is.na(est$FINS_PISOSHOGAR)] <- names(
  table(est$FINS_PISOSHOGAR)[which.max(table(est$FINS_PISOSHOGAR))])
# FINS_PISOSHOGAR
est$FINS_PISOSHOGAR <- ifelse(est$FINS_PISOSHOGAR=="1", "Pisos_Tierra", 
                              ifelse(est$FINS_PISOSHOGAR=="2", "Pisos_Cemento",
                                     ifelse(est$FINS_PISOSHOGAR=="3", "Pisos_Madera", "Pisos_Baldosa_Marmol")))
muestra3etapas$FINS_PISOSHOGAR <- ifelse(muestra3etapas$FINS_PISOSHOGAR=="1", "Pisos_Tierra", 
                                         ifelse(muestra3etapas$FINS_PISOSHOGAR=="2", "Pisos_Cemento",
                                                ifelse(muestra3etapas$FINS_PISOSHOGAR=="3", "Pisos_Madera", 
                                                       "Pisos_Baldosa_Marmol")))

est$FINS_TIENEINTERNET[is.na(est$FINS_TIENEINTERNET)] <- names(
  table(est$FINS_TIENEINTERNET)[which.max(table(est$FINS_TIENEINTERNET))])
est$FINS_TIENECOMPUTADOR[is.na(est$FINS_TIENECOMPUTADOR)] <- names(
  table(est$FINS_TIENECOMPUTADOR)[which.max(table(est$FINS_TIENECOMPUTADOR))])
est$FINS_TIENEAUTOMOVILPARTICULAR[is.na(est$FINS_TIENEAUTOMOVILPARTICULAR)] <- names(table(
  est$FINS_TIENEAUTOMOVILPARTICULAR)[which.max(table(est$FINS_TIENEAUTOMOVILPARTICULAR))])

est$LECTURA_CRITICA_PUNT[is.na(est$LECTURA_CRITICA_PUNT)]
est$LECTURA_CRITICA_PUNT <- as.numeric(est$LECTURA_CRITICA_PUNT)
est$MATEMATICAS_PUNT[is.na(est$MATEMATICAS_PUNT)]
est$MATEMATICAS_PUNT <- as.numeric(est$MATEMATICAS_PUNT)
est$SOCIALES_CIUDADANAS_PUNT[is.na(est$SOCIALES_CIUDADANAS_PUNT)]
est$SOCIALES_CIUDADANAS_PUNT <- as.numeric(est$SOCIALES_CIUDADANAS_PUNT)
est$CIENCIAS_NATURALES_PUNT[is.na(est$CIENCIAS_NATURALES_PUNT)]
est$CIENCIAS_NATURALES_PUNT <- as.numeric(est$CIENCIAS_NATURALES_PUNT)
est$INGLES_PUNT[is.na(est$INGLES_PUNT)]
est$INGLES_PUNT <- as.numeric(est$INGLES_PUNT)
est$INGLES_DESEM[is.na(est$INGLES_DESEM)]
est$EVALUADOS[is.na(est$EVALUADOS)]


muestra3etapas$CALENDARIO <- as.factor(muestra3etapas$CALENDARIO)
muestra3etapas$NATURALEZA <- as.factor(muestra3etapas$NATURALEZA)
muestra3etapas$JORNADA <- as.factor(muestra3etapas$JORNADA)
muestra3etapas$PERS_GENERO <- as.factor(muestra3etapas$PERS_GENERO)
muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA <- as.factor(muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA)
muestra3etapas$FINS_PISOSHOGAR <- as.factor(muestra3etapas$FINS_PISOSHOGAR)
muestra3etapas$FINS_TIENEINTERNET <- as.factor(muestra3etapas$FINS_TIENEINTERNET)
muestra3etapas$FINS_TIENECOMPUTADOR <- as.factor(muestra3etapas$FINS_TIENECOMPUTADOR)
muestra3etapas$FINS_TIENEAUTOMOVILPARTICULAR <- as.factor(muestra3etapas$FINS_TIENEAUTOMOVILPARTICULAR)
muestra3etapas$INGLES_DESEM <- as.factor(muestra3etapas$INGLES_DESEM)


#*****************************
# Información auxiliar ####
#*****************************
