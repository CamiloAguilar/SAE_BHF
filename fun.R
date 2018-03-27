
## Genera dummies con la función Domains() de 'teachingsampling'
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


## Realiza un modelo para cada combinación posible de variables
fit.BHF <- function(muestra, pop, grupo_vars, dominio, n_bootstrap, semilla){
  modelos <- list()
  num_mod <- 1
  for (i in 1:length(grupo_vars$Gr1)) {
    for (j in 1:length(grupo_vars$Gr3)) {
      for(k in 1:length(grupo_vars$Gr4)){
        # Define variables a usar para cada ejecución del modelo
        variables <- c("CODIGOMUNICIPIO", grupo_vars$Gr1[i], grupo_vars$Gr2, grupo_vars$Gr3[j], grupo_vars$Gr4[k])
        db <- pop %>% dplyr::select(variables)
        
        # Define dummies sobre info auxiliar
        dummies <- fit.dummies(db)
        Medias <- dummies$db %>% group_by(CODIGOMUNICIPIO) %>%
          summarise_all(funs(mean), na.rm=T)
        names(Medias)[2:length(names(Medias))] <- paste0("Prop_", names(Medias)[2:length(names(Medias))])
        
        Tamanos <- dummies$db %>% group_by(CODIGOMUNICIPIO) %>%
          summarise(N_d = n())
        InfoAux <- merge(Medias, Tamanos, by="CODIGOMUNICIPIO")
        
        #******************************
        ## A  justa modelo
        #******************************
        nombres <- dummies$variables[-1]
        formula <- nombres[1]
        for(m in 2:length(nombres)){
          formula <- paste(formula, nombres[m], sep=" + ")
        }
        formula <- as.formula(paste0("MATEMATICAS_PUNT ~ ", formula))
        
        set.seed(semilla)
        BHF <- pbmseBHF(formula, 
                        dom = CODIGOMUNICIPIO, 
                        meanxpop = Medias,
                        popnsize = Tamanos,
                        B = n_bootstrap, data = muestra)
        
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
        modelo_mixto <- lme(formula, random = ~1 | as.factor(CODIGOMUNICIPIO), data = muestra)
        Varest_betaest <- vcov(modelo_mixto)
        sigma2est_u <- BHF[[1]]$fit$refvar
        
        # Identificar los dominios no observados
        dominios_noobservados <- unique(est$CODIGOMUNICIPIO)[!(unique(est$CODIGOMUNICIPIO) %in% 
                                                                 unique(muestra$CODIGOMUNICIPIO))]
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
        
        # Resultados finales
        
        Resultados <- merge(Prom_dominios, df_MSE_Dominios, by = "MUNICIPIO")
        Resultados$Yhat_BHF <- ifelse(Resultados$ClaseDominio == "No observado", Resultados$Ybar_efectosfijos,
                                      Resultados$eblup)
        Resultados$cve <- 100 * sqrt(Resultados$MSE) / Resultados$Yhat_BHF
        
        mean_cve <- c(Gr1[i], Gr2, Gr3[j], Gr4[k], mean(Resultados$cve), mean(Resultados$MSE))
        
        res_mod <- list(Resultados, mean_cve)
        names(res_mod) <- c("Resultados", "mean_cve_mse")
        
        modelos[[num_mod]] <- res_mod
        names(modelos)[num_mod] <- paste0("modelo_", num_mod)
        num_mod <- num_mod + 1
      }
    }
  }
  
  ## Resultados de todas las iteraciones
  return(modelos)
}