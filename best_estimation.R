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

#****************
# fit.dummies
#****************
fit.dummies <- function(db){
  require(stringr)
  res <- list()
  original_nm <- names(db) 
  tipos <- sapply(db[,2:(dim(db)[2])], class)
  tipos <- tipos[which(!(tipos%in%c("numeric", "integer")))]
  for(l in 1:length(tipos)){
    y <- Domains(db[,names(tipos)[l]])
    
    # Define nombre de lar variables dummies
    if(is.null(colnames(y))){
      colnames(y) <- paste(str_sub(names(tipos)[l], 1, 8), 1:(dim(y)[2]), sep="_")
    } else{
      colnames(y) <- paste(str_sub(names(tipos)[l], 1, 8), colnames(y), sep="_")
    }
    #colnames(y) <- str_replace_all(colnames(y), " ", "_")
    
    # Pega variables a la base
    y<-y[,-1]
    db <- cbind(db, y)
  }
  
  # Elimina variables originales
  quitar <- which(names(db) %in% names(tipos))
  name_quita <- names(db)[quitar]
  original_nm <- c(original_nm[-(quitar)], name_quita)
  db <- db[, -quitar]
  
  # Resultado
  res[[1]] <- db
  res[[2]] <- original_nm
  names(res) <- c("db", "variables")
  
  return(res)
}

#*****************************
# Información auxiliar ####
#*****************************

# Define dummies sobre info auxiliar
variables <- c("CODIGOMUNICIPIO", "CIENCIAS_NATURALES_PUNT", "FINS_ESTRATOVIVIENDAENERGIA", 
               "CALENDARIO", "INGLES_DESEM")
db <- est %>% dplyr::select(variables)
dummies <- fit.dummies(db)
Medias <- dummies$db %>% group_by(CODIGOMUNICIPIO) %>%
          summarise_all(funs(mean), na.rm=T)
names(Medias)[2:length(names(Medias))] <- paste0("Prop_", names(Medias)[2:length(names(Medias))])

Tamanos <- dummies$db %>% group_by(CODIGOMUNICIPIO) %>%
           summarise(N_d = n())
InfoAux <- merge(Medias, Tamanos, by="CODIGOMUNICIPIO")

#******************************
## Ajusta modelo
#******************************
nombres <- dummies$variables[-1]
formula <- nombres[1]
for(m in 2:length(nombres)){
  formula <- paste(formula, nombres[m], sep=" + ")
}
formula <- as.formula(paste0("MATEMATICAS_PUNT ~ ", formula))

set.seed(12345)
BHF <- pbmseBHF(formula, 
                dom = CODIGOMUNICIPIO, 
                meanxpop = Medias,
                popnsize = Tamanos,
                B = 200, data = muestra3etapas)

#******************************
## Resultados del modelo
#******************************

# Estimaciones para dominios externos (que no salieron en la muestra)
Beta_est <- BHF$est$fit$fixed
names(Beta_est) <- gsub("XsXs", "", names(Beta_est) )
names(Beta_est)[1] <-" Intercepto" 
Beta_est <- as.matrix(Beta_est)

# Totales por dominio
Xbar_d <- Medias[,-1]
Unos <- as.data.frame(as.matrix(rep(1, nrow(InfoAux))))
Xbar_d <- cbind(Unos, Xbar_d)
Xbar_d <- as.matrix(Xbar_d)
rownames(Xbar_d) <- InfoAux$CODIGOMUNICIPIO

dim(Xbar_d); dim(Beta_est)
Prom_dominios <- Xbar_d %*% Beta_est
rownames(Prom_dominios) <- InfoAux$CODIGOMUNICIPIO
Prom_dominios <- as.data.frame(Prom_dominios)
Prom_dominios$domain <- row.names(Prom_dominios)
colnames(Prom_dominios)[1] <- "Ybar_efectosfijos" 

# Conservar los dominios no observados 
Prom_dominios_observados <- BHF$est$eblup
Prom_dominios <- merge(Prom_dominios, Prom_dominios_observados, by = "domain", all.x = T)
names(Prom_dominios)[1] <- "MUNICIPIO"

#*****************************************************
## Estimación MSE para dominios no observados 
#*****************************************************
modelo_mixto <- lme(formula, random = ~1 | as.factor(CODIGOMUNICIPIO), data = muestra3etapas)
Varest_betaest <- vcov(modelo_mixto)
sigma2est_u <- BHF[[1]]$fit$refvar

# Identificar los dominios no observados
dominios_noobservados <- unique(est$CODIGOMUNICIPIO)[!(unique(est$CODIGOMUNICIPIO) %in% 
                                                         unique(muestra3etapas$CODIGOMUNICIPIO))]
Xbar_d_noobs <- Xbar_d[row.names(Xbar_d) %in% dominios_noobservados,]
MSE_DominiosNoobservados <- diag((Xbar_d_noobs %*% Varest_betaest %*% t(Xbar_d_noobs)) + sigma2est_u)
MSE_DominiosNoobservados <- as.table(MSE_DominiosNoobservados)
df_MSE_DominiosNoobservados <- as.data.frame(MSE_DominiosNoobservados)
names(df_MSE_DominiosNoobservados) <- c("MUNICIPIO", "MSE")
df_MSE_DominiosNoobservados$ClaseDominio <- "No observado"

df_MSE_Dominiosobservados <- BHF$mse
names(df_MSE_Dominiosobservados) <- c("MUNICIPIO", "MSE")
df_MSE_Dominiosobservados$ClaseDominio <- "Observado"

dim(df_MSE_DominiosNoobservados); dim(df_MSE_Dominiosobservados)
df_MSE_Dominios <- bind_rows(df_MSE_DominiosNoobservados, df_MSE_Dominiosobservados)
df_MSE_Dominios <- df_MSE_Dominios[order(df_MSE_Dominios$MUNICIPIO),]

#****************************
# Resultados finales
#****************************
Resultados <- merge(Prom_dominios, df_MSE_Dominios, by = "MUNICIPIO")
Resultados$Yhat_BHF <- ifelse(Resultados$ClaseDominio == "No observado", Resultados$Ybar_efectosfijos,
                              Resultados$eblup)
Resultados$cve <- 100 * sqrt(Resultados$MSE) / Resultados$Yhat_BHF
head(Resultados)
mean(Resultados$cve); mean(Resultados$MSE)

mean_cve <- c("CIENCIAS_NATURALES_PUNT", "FINS_ESTRATOVIVIENDAENERGIA", 
              "CALENDARIO", "INGLES_DESEM", mean(Resultados$cve), mean(Resultados$MSE))
mean_cve

write.table(Resultados, "Resultados.csv", row.names = F, sep=";")





