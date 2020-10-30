
VarG <- function(model, comp){
  v <- as.data.frame(VarCorr(model))
  v <- v[v$grp==comp,"vcov"]
  return(v)
}

VarE <- function(model){
  v <- as.data.frame(VarCorr(model))
  v <- v[v$grp=="Residual","vcov"]
  return(v)
}

h.cullis <- function(model, gen){
  aveped <- mean(attr(ranef(model,drop=T)[[gen]],"postVar"))
  vc.g <- as.data.frame(VarCorr(model))
  vc.g <- vc.g[vc.g$grp==gen,"vcov"]
  ifelse(vc.g==0, 0 , round(1-aveped/vc.g,2) )
}

ran <- function(var){
  effect <- paste0("(",1,"|",var,")")
  return(effect)
}


lme4_single <- function(data, response, genotype, res_ran, model, replicate, block, covariate, formula){
  
  dt <- data
  dt$Response  <- dt[ ,response] 
  dt$Gen       <- as.factor(dt[ ,genotype] )
  
  if(res_ran==TRUE){
    if (model==1) {                                # Alpha-lattice
      dt$Replicate <-as.factor(dt[ ,replicate]  )
      dt$Block  <-as.factor(dt[ ,block] )
      equation <- reformulate(c(ran("Gen"), "Replicate", ran("Replicate:Block"), covariate), response = "Response")
      Modelo <-  lmerTest::lmer(equation,data=dt)
    }
    else if (model==2)   {                         # Bloques
      dt$Replicate <- as.factor(dt[ ,replicate]  )
      equation <- reformulate(c(ran("Gen"), "Replicate", covariate), response = "Response")
      Modelo = lmerTest::lmer(equation, data = dt)
    }
    else if (model==3)  {                          # Free
      gen <- ran(genotype)
      if (formula!="") {
        gen <-  paste0(gen," + ")
      } else { gen <- gen }
      equation <- as.formula(paste0(response," ~ ", gen, formula))
      Modelo = try(lmerTest::lmer(formula = equation , data=dt ),silent = TRUE)
      tryCatch({ if(class(Modelo)=="try-error") stop("Error in formula") },
               error = function(e) { shinytoastr::toastr_error(title = "Warning:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)})
      validate(need(class(Modelo)!="try-error", "Check the formula"))
      Modelo     }
  }
  
  if(res_ran==FALSE) {
    if (model==1){ 
      dt$Replicate <-as.factor(dt[ ,replicate]  )
      dt$Block  <-as.factor(dt[ ,block]     )
      equation <- reformulate(c("Gen", "Replicate", ran("Replicate:Block"), covariate), response = "Response")
      Modelo = lmerTest::lmer(equation, data=dt)
      Modelo }
    else if (model==2)   {
      dt$Replicate <-as.factor(dt[ ,replicate]  )
      equation <- reformulate(c("Gen", ran("Replicate"), covariate), response = "Response")
      Modelo = lmerTest::lmer(equation,data=dt)
      Modelo }
    else if (model==3)  {  
      gen <-  paste(genotype)
      if (formula!="") {
        gen <-  paste0(gen," + ")
      } else { gen <- gen }
      
      equation <- as.formula(paste0(response," ~ ", gen, formula))
      Modelo = try(lmerTest::lmer(formula = equation , data=dt ),silent = TRUE)
      tryCatch({ if(class(Modelo)=="try-error") stop("Error in formula")},
               error = function(e) { shinytoastr::toastr_error(title = "Warning:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)})
      validate(need(class(Modelo)!="try-error", "Check the formula"))
      Modelo     
    }
  }
  
  return(Modelo)
}




lme4_effects <- function(model, genotype, res_ran, model_class){
  if (res_ran==TRUE) {
    if (model_class==3) {
      BLUPS <- ranef(model)[[genotype]]#+fixef(model)[1]
      BLUPS <- data.frame(as.factor(row.names(BLUPS)),BLUPS[,1])
      colnames(BLUPS) <- c("Genotype","Effect")
      BLUPS <- dplyr::arrange(BLUPS,desc(Effect))
      BLUPS <- data.frame(BLUPS[,1],round(BLUPS[,2],2))
      names(BLUPS) <- c("Line","BLUPs")
      d <- broom.mixed::augment(ranef(model))
      d <- d[d$grp==genotype,c("level","std.error")]
      d <- data.frame(level=d[,1],std.error=round(d[,2],2))
      BLUPS <- merge(BLUPS,d,by.x="Line",by.y="level")
      BLUPS
    } else {     
      BLUPS <- ranef(model)[["Gen"]]#+fixef(model)[1]
      BLUPS <- data.frame(as.factor(row.names(BLUPS)),BLUPS[,1])
      colnames(BLUPS) <- c("Genotype","Effect")
      BLUPS <- dplyr::arrange(BLUPS,desc(Effect))
      BLUPS <- data.frame(BLUPS[,1],round(BLUPS[,2],2))
      names(BLUPS) <- c("Line","BLUPs")
      d <- broom.mixed::augment(ranef(model))
      d <- d[d$grp=="Gen",c("level","std.error")]
      d <- data.frame(level=d[,1],std.error=round(d[,2],2))
      BLUPS <- merge(BLUPS,d,by.x="Line",by.y="level")
      BLUPS
    }
  } else {
    if (model_class==3) {
      BLUES <- data.frame(lmerTest::ls_means(model,genotype))
      BLUES <-  dplyr::arrange(BLUES,desc(Estimate))
      BLUES
    }
    BLUES <- data.frame(lmerTest::ls_means(model,"Gen"))  # "Gen"  fue cambiado por input$genotipo2
    BLUES <-  dplyr::arrange(BLUES,desc(Estimate))
    BLUES <- BLUES %>%
              dplyr::mutate_if(is.numeric, round, digits=2) %>%
              dplyr::select(levels, Estimate, "Std..Error", lower, upper )
    names(BLUES) <- c("Genotype","Estimation", "Std.Error", "lower", "upper")
    BLUES
  }
}



res_data_lme4 <- function(Model){
  Data <- Model@frame
  VarE <- VarE(Model)  
  Data$Index <- 1:length(residuals(Model))
  Data$Residuals <- residuals(Model)
  u <- +3*sqrt(VarE)
  l <- -3*sqrt(VarE)
  Data$Classify <- NA
  Data$Classify[which(abs(Data$Residuals)>=u)] <- "Outlier" 
  Data$Classify[which(abs(Data$Residuals)<u)  ] <- "Normal"
  Data$l <- l
  Data$u <- u
  Data$fit <-  fitted.values(Model)
  return(Data)
}








