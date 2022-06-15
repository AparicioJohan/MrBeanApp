
VarG <- function(model, comp) {
  v <- as.data.frame(VarCorr(model))
  v <- v[v$grp == comp, "vcov"]
  return(v)
}

VarE <- function(model) {
  if (class(model) == "lm") {
    v <- sigma(model)^2
  } else {
    v <- as.data.frame(VarCorr(model))
    v <- v[v$grp == "Residual", "vcov"]
  }
  return(v)
}

h.cullis <- function(model, gen) {
  aveped <- mean(attr(ranef(model, drop = T)[[gen]], "postVar"))
  vc.g <- as.data.frame(VarCorr(model))
  vc.g <- vc.g[vc.g$grp == gen, "vcov"]
  ifelse(vc.g == 0, 0, round(1 - aveped / vc.g, 2))
}

ran <- function(var) {
  effect <- paste0("(", 1, "|", var, ")")
  return(effect)
}


lme4_single <- function(data,
                        response,
                        genotype,
                        res_ran,
                        model,
                        replicate,
                        rep_ran = FALSE,
                        block,
                        block_ran = TRUE,
                        col = "",
                        col_ran = TRUE,
                        row = "",
                        row_ran = TRUE,
                        covariate,
                        formula) {
  dt <- data
  dt$Response <- dt[, response]
  dt$Gen <- as.factor(dt[, genotype])

  if (res_ran == TRUE) {
    if (model == 1) { # Alpha-lattice
      dt$Replicate <- as.factor(dt[, replicate])
      dt$Block <- as.factor(dt[, block])
      equation <- reformulate(c(
        ran("Gen"),
        ifelse(rep_ran, ran("Replicate"), "Replicate"),
        ifelse(block_ran, ran("Replicate:Block"), "Replicate:Block"),
        covariate
      ),
      response = "Response"
      )
      Modelo <- lmerTest::lmer(equation, data = dt)
    } else if (model == 2) { # Bloques
      dt$Replicate <- as.factor(dt[, replicate])
      equation <- reformulate(c(
        ran("Gen"),
        ifelse(rep_ran, ran("Replicate"), "Replicate"),
        covariate
      ),
      response = "Response"
      )
      Modelo <- lmerTest::lmer(equation, data = dt)
    } else if (model == 3) { # Free
      gen <- ran(genotype)
      if (formula != "") {
        gen <- paste0(gen, " + ")
      } else {
        gen <- gen
      }
      equation <- as.formula(paste0(response, " ~ ", gen, formula))
      Modelo <- try(lmerTest::lmer(formula = equation, data = dt), silent = TRUE)
      tryCatch(
        {
          if (class(Modelo) == "try-error") stop("Error in formula")
        },
        error = function(e) {
          shinytoastr::toastr_error(title = "Warning:", conditionMessage(e), position = "bottom-right", progressBar = TRUE)
        }
      )
      validate(need(class(Modelo) != "try-error", "Check the formula"))
      Modelo
    } else if (model == 4) { # CRD
      equation <- reformulate(c(ran("Gen"), covariate), response = "Response")
      Modelo <- lmerTest::lmer(equation, data = dt)
    } else if (model == 5) {
      dt$col_f <- as.factor(dt[, col])
      dt$row_f <- as.factor(dt[, row])
      if (replicate != "") {
        dt$Replicate <- as.factor(dt[, replicate])
        equation <- reformulate(c(
          ran("Gen"),
          ifelse(rep_ran, ran("Replicate"), "Replicate"),
          ifelse(col_ran, ran("Replicate:col_f"), "Replicate:col_f"),
          ifelse(row_ran, ran("Replicate:row_f"), "Replicate:row_f"),
          covariate
        ),
        response = "Response"
        )
      } else {
        equation <- reformulate(c(
          ran("Gen"),
          ifelse(col_ran, ran("col_f"), "col_f"),
          ifelse(row_ran, ran("row_f"), "row_f"),
          covariate
        ),
        response = "Response"
        )
      }
      Modelo <- lmerTest::lmer(equation, data = dt)
    }
  }

  if (res_ran == FALSE) {
    if (model == 1) {
      dt$Replicate <- as.factor(dt[, replicate])
      dt$Block <- as.factor(dt[, block])
      if (rep_ran & block_ran) {
        equation <- reformulate(c("Gen", ran("Replicate"), ran("Replicate:Block"), covariate), response = "Response")
        Modelo <- lmerTest::lmer(equation, data = dt)
      } else if (rep_ran & !block_ran) {
        equation <- reformulate(c("Gen", ran("Replicate"), "Replicate:Block", covariate), response = "Response")
        Modelo <- lmerTest::lmer(equation, data = dt)
      } else if (!rep_ran & block_ran) {
        equation <- reformulate(c("Gen", "Replicate", ran("Replicate:Block"), covariate), response = "Response")
        Modelo <- lmerTest::lmer(equation, data = dt)
      } else {
        equation <- reformulate(c("Gen", "Replicate", "Replicate:Block", covariate), response = "Response")
        Modelo <- lm(equation, data = dt)
      }
      Modelo
    } else if (model == 2) {
      dt$Replicate <- as.factor(dt[, replicate])
      if (rep_ran) {
        equation <- reformulate(c("Gen", ran("Replicate"), covariate), response = "Response")
        Modelo <- lmerTest::lmer(equation, data = dt)
      } else {
        equation <- reformulate(c("Gen", "Replicate", covariate), response = "Response")
        Modelo <- lm(equation, data = dt)
      }
      Modelo
    } else if (model == 3) {
      gen <- paste(genotype)
      if (formula != "") {
        gen <- paste0(gen, " + ")
      } else {
        gen <- gen
      }

      equation <- as.formula(paste0(response, " ~ ", gen, formula))
      ind <- sum(grepl("(", equation, fixed = TRUE)) == 0
      if (ind) {
        Modelo <- try(lm(formula = equation, data = dt), silent = TRUE)
      } else {
        Modelo <- try(lmerTest::lmer(formula = equation, data = dt), silent = TRUE)
      }

      tryCatch(
        {
          if (class(Modelo) == "try-error") stop("Error in formula")
        },
        error = function(e) {
          shinytoastr::toastr_error(title = "Warning:", conditionMessage(e), position = "bottom-right", progressBar = TRUE)
        }
      )
      validate(need(class(Modelo) != "try-error", "Check the formula"))
      Modelo
    } else if (model == 4) { # CRD
      equation <- reformulate(c("Gen", covariate), response = "Response")
      Modelo <- lm(equation, data = dt)
    } else if (model == 5) {
      dt$col_f <- as.factor(dt[, col])
      dt$row_f <- as.factor(dt[, row])
      if (replicate != "") {
        dt$Replicate <- as.factor(dt[, replicate])
        equation <- reformulate(c(
          "Gen",
          ifelse(rep_ran, ran("Replicate"), "Replicate"),
          ifelse(col_ran, ran("Replicate:col_f"), "Replicate:col_f"),
          ifelse(row_ran, ran("Replicate:row_f"), "Replicate:row_f"),
          covariate
        ),
        response = "Response"
        )
      } else {
        equation <- reformulate(c(
          "Gen",
          ifelse(col_ran, ran("col_f"), "col_f"),
          ifelse(row_ran, ran("row_f"), "row_f"),
          covariate
        ),
        response = "Response"
        )
      }
      ind <- sum(grepl("(", equation, fixed = TRUE)) == 0
      if (ind) {
        Modelo <- lm(formula = equation, data = dt)
      } else {
        Modelo <- lmerTest::lmer(formula = equation, data = dt)
      }
    }
  }

  return(Modelo)
}



lme4_effects <- function(model, genotype, res_ran, model_class) {
  if (res_ran == TRUE) {
    if (model_class == 3) {
      BLUPS <- ranef(model)[[genotype]] + mean(model@frame[[1]], na.rm = T)
      BLUPS <- data.frame(as.factor(row.names(BLUPS)), BLUPS[, 1])
      colnames(BLUPS) <- c("Genotype", "Effect")
      BLUPS <- dplyr::arrange(BLUPS, desc(Effect))
      BLUPS <- data.frame(BLUPS[, 1], round(BLUPS[, 2], 2))
      names(BLUPS) <- c("Line", "BLUPs")
      d <- broom.mixed::augment(ranef(model))
      d <- d[d$grp == genotype, c("level", "std.error")]
      d <- data.frame(level = d[, 1], std.error = round(d[, 2], 2))
      BLUPS <- merge(BLUPS, d, by.x = "Line", by.y = "level")
      BLUPS
    } else {
      BLUPS <- ranef(model)[["Gen"]] + mean(model@frame[[1]], na.rm = T)
      BLUPS <- data.frame(as.factor(row.names(BLUPS)), BLUPS[, 1])
      colnames(BLUPS) <- c("Genotype", "Effect")
      BLUPS <- dplyr::arrange(BLUPS, desc(Effect))
      BLUPS <- data.frame(BLUPS[, 1], round(BLUPS[, 2], 2))
      names(BLUPS) <- c("Line", "BLUPs")
      d <- broom.mixed::augment(ranef(model))
      d <- d[d$grp == "Gen", c("level", "std.error")]
      d <- data.frame(level = d[, 1], std.error = round(d[, 2], 2))
      BLUPS <- merge(BLUPS, d, by.x = "Line", by.y = "level")
      BLUPS
    }
  } else {
    if (model_class == 3) {
      if (class(model) == "lm") {
        form_equa <- reformulate(termlabels = genotype)
        BLUES <- data.frame(emmeans::emmeans(model, form_equa)) # Instalar emmeans
        BLUES <- BLUES %>%
          dplyr::select(-df) %>%
          dplyr::mutate_if(is.numeric, round, digits = 2)
        names(BLUES) <- c("Genotype", "Estimation", "Std.Error", "lower", "upper")
        BLUES
      } else {
        BLUES <- data.frame(lmerTest::ls_means(model, genotype))
        BLUES <- dplyr::arrange(BLUES, desc(Estimate))
        BLUES <- BLUES %>%
          dplyr::mutate_if(is.numeric, round, digits = 2) %>%
          dplyr::select(levels, Estimate, "Std..Error", lower, upper)
        names(BLUES) <- c("Genotype", "Estimation", "Std.Error", "lower", "upper")
        BLUES
      }
    } else {
      if (class(model) == "lm") {
        BLUES <- data.frame(emmeans::emmeans(model, ~Gen)) # Instalar emmeans
        BLUES <- BLUES %>%
          dplyr::select(-df) %>%
          dplyr::mutate_if(is.numeric, round, digits = 2)
        names(BLUES) <- c("Genotype", "Estimation", "Std.Error", "lower", "upper")
        BLUES
      } else {
        BLUES <- data.frame(lmerTest::ls_means(model, "Gen")) # "Gen"  fue cambiado por input$genotipo2
        BLUES <- dplyr::arrange(BLUES, desc(Estimate))
        BLUES <- BLUES %>%
          dplyr::mutate_if(is.numeric, round, digits = 2) %>%
          dplyr::select(levels, Estimate, "Std..Error", lower, upper)
        names(BLUES) <- c("Genotype", "Estimation", "Std.Error", "lower", "upper")
        BLUES
      }
    }
  }
}



res_data_lme4 <- function(Model) {
  if (class(Model) == "lm") {
    Data <- Model$model
    VarE <- sigma(Model)^2
  } else {
    Data <- Model@frame
    VarE <- VarE(Model)
  }
  Data$Index <- 1:length(residuals(Model))
  Data$Residuals <- residuals(Model)
  u <- +3 * sqrt(VarE)
  l <- -3 * sqrt(VarE)
  Data$Classify <- NA
  Data$Classify[which(abs(Data$Residuals) >= u)] <- "Outlier"
  Data$Classify[which(abs(Data$Residuals) < u)] <- "Normal"
  Data$l <- l
  Data$u <- u
  Data$fit <- fitted.values(Model)
  return(Data)
}




mult_comp <- function(model, res_ran, genotype, model_class, ngen) {
  if (ngen > 50) {
    return()
  }
  if (res_ran == TRUE) {
    return()
  } else {
    if (model_class == 3) {
      form_equa <- reformulate(termlabels = genotype, response = "pairwise")
      pair <- model %>%
        emmeans::emmeans(form_equa, adjust = "tukey") %>%
        .$contrasts %>%
        data.frame() %>%
        dplyr::mutate_if(is.numeric, round, digits = 3)
    } else {
      pair <- model %>%
        emmeans::emmeans(pairwise ~ "Gen", adjust = "tukey") %>%
        .$contrasts %>%
        data.frame() %>%
        dplyr::mutate_if(is.numeric, round, digits = 3)
    }
  }
  return(pair)
}
