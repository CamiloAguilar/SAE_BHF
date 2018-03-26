rm(list=ls())
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
# Únicamente podemos seleccionar 4 variables. Variable objetivo:= Puntaje matemáticas
Obj <- "MATEMATICAS_PUNT"

# Puntaje materias
Gr1 <- c("LECTURA_CRITICA_PUNT", "SOCIALES_CIUDADANAS_PUNT", "CIENCIAS_NATURALES_PUNT", "INGLES_PUNT")

# Estrato socieconómico
Gr2 <- c("FINS_ESTRATOVIVIENDAENERGIA")

# Asociadas al colegio
Gr3 <- c("CALENDARIO", "NATURALEZA", "JORNADA") 

# Otras; todas las demás
Gr4 <- c("FINS_PERSONASHOGARACTUAL", "FINS_CUARTOSHOGARACTUAL", "FINS_PISOSHOGAR", "FINS_TIENEINTERNET",
         "FINS_TIENECOMPUTADOR", "FINS_TIENEAUTOMOVILPARTICULAR", "INGLES_DESEM")

i<-1; j<-1; k<-1; l<-1
pop <- est
muestra <- muestra3etapas

modelos <- list()
num_mod <- 1
for (i in 1:length(Gr1)) {
  for (j in 1:length(Gr3)) {
    for(k in 1:length(Gr4)){
      # Define variables a usar para cada ejecución del modelo
      variables <- c("CODIGOMUNICIPIO", Gr1[i], Gr2, Gr3[j], Gr4[k])
      db <- pop %>% dplyr::select(variables)
      
      # Define dummies sobre info auxiliar
      dummies <- fit.dummies(db)
      Medias <- dummies$db %>% group_by(CODIGOMUNICIPIO) %>%
                summarise_all(funs(mean), na.rm=T)
      names(Medias)[2:length(names(Medias))] <- paste0("Prop_", names(Medias)[2:length(names(Medias))])
      
      Tamanos <- dummies$db %>% group_by(CODIGOMUNICIPIO) %>%
                 summarise(N_d = n())
      
      # ajusta modelo
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
                      B = 50, data = muestra)
    }
  }
}

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









