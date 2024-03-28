#' R.square
#'
#' @param Model SpATS object
#'
#' @return a numeric value
#' @noRd
R.square <- function(Model) {
  response <- Model$data[, Model$model$response]
  mean.response <- mean(response, na.rm = TRUE)
  fitted <- Model$fitted
  SS.fitted <- sum((response - fitted)^2, na.rm = TRUE)
  SS.response <- sum((response - mean.response)^2, na.rm = TRUE)
  R <- 1 - SS.fitted / SS.response
  names(R) <- "r.square"
  return(round(R, 3))
}

#' Coefficient of Variation SpATS
#'
#' @param Model SpATS object
#' 
#' @importFrom stats median
#'
#' @return a numeric value
#' @noRd
CV.spats <- function(Model) {
  response <- Model$data[, Model$model$response]
  mean.response <- mean(response, na.rm = TRUE)
  fitted <- Model$fitted
  MSE <- mean((response - fitted)^2, na.rm = TRUE)
  RMSE <- sqrt(MSE)
  NRMSE <- RMSE / mean.response
  cv_prcnt <- NRMSE * 100
  names(cv_prcnt) <- "CV"
  return(round(cv_prcnt, 2))
}

#' Residuals
#'
#' @param Model SpATS object
#'
#' @return a data.frame
#' @noRd
res_data <- function(Model, k = 3) {
  dt <- Model$data
  VarE <- Model$psi[1]
  Data <- data.frame(Index = 1:length(stats::residuals(Model)), Residuals = stats::residuals(Model))
  u <- +k * sqrt(VarE)
  l <- -k * sqrt(VarE)
  Data$Classify <- NA
  Data$Classify[which(abs(Data$Residuals) >= u)] <- "Outlier"
  Data$Classify[which(abs(Data$Residuals) < u)] <- "Normal"
  Data$l <- l
  Data$u <- u
  Data$gen <- dt[, Model$model$geno$genotype]
  Data$col <- dt[, Model$terms$spatial$terms.formula$x.coord]
  Data$row <- dt[, Model$terms$spatial$terms.formula$y.coord]
  Data$fit <- stats::fitted.values(Model)
  Data$response <- dt[, Model$model$response]
  return(Data)
}

#' Residuals against Index plot
#'
#' @param data_out
#'
#' @return plotly object
#' @noRd
res_index <- function(data_out) {
  k <- dplyr::filter(data_out, !is.na(Classify)) %>%
    ggplot2::ggplot(ggplot2::aes(x = Index, y = Residuals, color = Classify)) +
    ggplot2::geom_point(size = 2, alpha = 0.5, na.rm = T) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values = c("grey80", "red")) +
    ggplot2::geom_hline(yintercept = data_out$u, color = "red") +
    ggplot2::geom_hline(yintercept = data_out$l, color = "red") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  plotly::ggplotly(k)
}

#' Residuals in the field
#'
#' @param data_out data.frame result from res_data
#'
#' @return plotly object
#' @noRd
res_map <- function(data_out) {
  k <- dplyr::filter(data_out, !is.na(Classify)) %>%
    ggplot2::ggplot(ggplot2::aes(x = col, y = row, color = Classify)) +
    ggplot2::geom_point(size = 2, na.rm = T) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values = c("grey80", "red"))
  plotly::ggplotly(k)
}

#' Residuals against Fitted values plot
#'
#' @param data_out data.frame result from res_data
#'
#' @return plotly object
#' @noRd
res_fitted <- function(data_out) {
  k <- dplyr::filter(data_out, !is.na(Classify)) %>%
    ggplot2::ggplot(ggplot2::aes(x = fit, y = Residuals, color = Classify)) +
    ggplot2::geom_point(size = 2, alpha = 0.5, na.rm = T) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values = c("grey80", "red")) +
    ggplot2::xlab("Fitted Values") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  plotly::ggplotly(k)
}

#' QQplot Residuals
#'
#' @param data_out data.frame result from res_data
#'
#' @return plotly object
#' @noRd
res_qqplot <- function(data_out) {
  q <- dplyr::filter(data_out, !is.na(Classify)) %>%
    ggpubr::ggqqplot(
      x = "Residuals",
      fill = "Classify",
      ggtheme = ggplot2::theme_bw(),
      ylab = "Sample Quantile",
      xlab = "Theoretical Quantile"
    )
  plotly::ggplotly(q)
}

#' Histogram Residuals
#'
#' @param data_out data.frame result from res_data
#'
#' @return plotly object
#' @noRd
res_hist <- function(data_out) {
  hi <- graphics::hist(data_out[, "Residuals"], plot = FALSE)
  br <- hi$breaks
  p <- ggplot2::ggplot(data_out, ggplot2::aes(x = Residuals)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), alpha = 0.8, breaks = c(br), na.rm = T) +
    ggplot2::theme_bw() +
    ggplot2::geom_density(alpha = 0.5, na.rm = T) +
    ggplot2::geom_vline(xintercept = c(data_out$u, data_out$l), linetype = 2, color = "red")
  plotly::ggplotly(p)
}

#' LSD SpATS
#'
#' @param model an object of class SpATS as produced by SpATS()
#' @param alpha Level of risk for the test (0.05 by default)
#' @param data.frame whether return only the LSD value or a complete summary
#'  FALSE by default
#'
#' @return data.frame or a lsd value depending on the data.frame argument
#' 
#' @importFrom stats median qt
#' @export
#'
#' @examples
#' # library(SpATS)
#' # data(wheatdata)
#' # wheatdata$R <- as.factor(wheatdata$row)
#' # wheatdata$C <- as.factor(wheatdata$col)
#' #
#' # m1 <- SpATS(
#' #  response = "yield",
#' #  spatial = ~ PSANOVA(col, row, nseg = c(10, 20), nest.div = 2),
#' #  genotype = "geno",
#' #  genotype.as.random = FALSE,
#' #  fixed = ~ colcode + rowcode,
#' #  random = ~ R + C,
#' #  data = wheatdata,
#' #  control = list(tolerance = 1e-03)
#' # )
#' # LSD(m1, data.frame = TRUE)
LSD <- function(model, alpha = 0.05, data.frame = FALSE) {
  gen_fixed <- !model$model$geno$as.random
  if (gen_fixed) {
    response <- model$model$response
    mean_resp <- mean(model$data[, response], na.rm = TRUE)
    var_E <- model$psi[1]
    n_obs <- model$nobs
    df <- n_obs - sum(model$eff.dim)
    n_reps <- model$terms$geno$geno_dim
    names(n_reps) <- model$terms$geno$geno_names
    ns <- stats::median(n_reps)
    t_value <- stats::qt(p = 1 - (alpha / 2), df = df)
    lsd <- t_value * sqrt(var_E * (1 / ns + 1 / ns))
    if (data.frame) {
      DT <- data.frame(
        var_E = var_E,
        Df = df,
        Mean = mean_resp,
        t_value = t_value, 
        alpha = alpha, 
        LSD = lsd
      )
      return(DT)
    } else {
      return(lsd)
    }
  } else {
    message("Genotype should be fixed.")
    return()
  }
}

res_compare <- function(Model, variable, factor) {
  data <- Model$data
  data$Residuals <-stats::residuals(Model)
  data <- utils::type.convert(data, as.is = FALSE)
  req(variable)
  label <- class(data[, variable])
  if (factor) {
    data[, variable] <- as.factor(data[, variable])
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = variable, y = "Residuals", fill = variable)) +
      ggplot2::geom_boxplot(na.rm = T) +
      ggplot2::theme_bw()
  } else {
    data[, variable] <- as.numeric(data[, variable])
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = variable, y = "Residuals")) +
      ggplot2::geom_point(size = 2, alpha = 0.5, color = "grey80", na.rm = T) +
      ggplot2::theme_bw()
  }
  plotly::ggplotly(p)
}


check_gen_SpATS <- function(gen, data, check_gen = c("ci", "st", "wa")) {
  data <- as.data.frame(data)
  indx <- sum(check_gen %in% data[, gen]) >= 1
  if (indx) {
    genotypes_id <- as.character(data[, gen])
    data[, gen] <- as.factor(ifelse(genotypes_id %in% check_gen, NA, genotypes_id))
    data$checks <- as.factor(ifelse(genotypes_id %in% check_gen, genotypes_id, "_NoCheck"))
  } else {
    message("No checks in this trial")
  }
  return(data)
}

#' @importFrom stats as.formula
SpATS_mrbean <- function(data = NULL, 
                         response = "",
                         genotype = "",
                         col = "", 
                         row = "", 
                         segm = FALSE,
                         ncols = NULL,
                         nrows = NULL,
                         rep = "",
                         fix_fact = "",
                         ran_fact = "", 
                         gen_ran = TRUE, 
                         covariate = NULL,
                         clean_out = FALSE, 
                         iterations = 1,
                         checks = NULL,
                         k_clean_out = 3) {
  dt <- data
  dt[, genotype] <- as.factor(dt[, genotype])
  dt$col <- dt[, col]
  dt$row <- dt[, row]
  dt$col_f <- factor(dt[, col])
  dt$row_f <- factor(dt[, row])
  ncols <- ifelse(
    test = is.null(ncols) | isFALSE(segm),
    yes = length(unique(dt[, col])), 
    no = ncols
  )
  nrows <- ifelse(
    test = is.null(nrows) | isFALSE(segm),
    yes = length(unique(dt[, row])),
    no = nrows
  )
  if (!is.null(fix_fact)) {
    for (i in 1:length(fix_fact)) {
      dt[, fix_fact[i]] <- as.factor(dt[, fix_fact[i]])
    }
  }
  if (!is.null(ran_fact)) {
    for (i in 1:length(ran_fact)) {
      dt[, ran_fact[i]] <- as.factor(dt[, ran_fact[i]])
    }
  }
  if (is.null(fix_fact) & is.null(covariate)) {
    Fijo <- as.formula(~NULL)
  } else if (!is.null(fix_fact) & is.null(covariate)) {
    Fijo <- as.formula(
      paste("", paste(fix_fact, collapse = " + "), sep = " ~ ")
    )
  } else if (!is.null(fix_fact) & !is.null(covariate)) {
    Fijo <- as.formula(
      paste("", paste(c(fix_fact, covariate), collapse = " + "), sep = " ~ ")
    )
  } else if (is.null(fix_fact) & !is.null(covariate)) {
    Fijo <- as.formula(
      paste("", paste(covariate, collapse = " + "), sep = " ~ ")
    )
  } 
  if (rep != "") {
    dt$rep <- as.factor(dt[, rep])
    if (nlevels(dt$rep) > 1) {
      if (is.null(ran_fact)) {
        Random <- as.formula(~ rep:col_f + rep:row_f)
      } else {
        Random <- as.formula(
          paste(
            "",
            paste(c(ran_fact, "rep:col_f", "rep:row_f"), collapse = " + "), 
            sep = " ~ "
          )
        )
      }
      if (is.null(fix_fact) & is.null(covariate)) {
        Fijo <- as.formula(~rep)
      } else {
        Fijo <- paste(paste0(as.character(Fijo), collapse = ""), "+ rep")
      }
    } else {
      if (is.null(ran_fact)) {
        Random <- as.formula(~ col_f + row_f)
      } else {
        Random <- as.formula(
          paste(
            "", 
            paste(c(ran_fact, "col_f", "row_f"), collapse = " + "),
            sep = " ~ "
          )
        )
      }
    }
  } else {
    if (is.null(ran_fact)) {
      Random <- as.formula(~ col_f + row_f)
    } else {
      Random <- as.formula(
        paste(
          "", 
          paste(c(ran_fact, "col_f", "row_f"), collapse = " + "),
          sep = " ~ "
        )
      )
    }
  }
  if (!is.null(checks) & gen_ran == TRUE) {
    dt <- check_gen_SpATS(gen = genotype, data = dt, check_gen = checks)
    if ("checks" %in% names(dt)) {
      if (is.null(fix_fact) & is.null(covariate) & rep == "") {
        Fijo <- as.formula(~checks)
      } else {
        Fijo <- paste(paste0(as.character(Fijo), collapse = ""), "+ checks")
      }
    }
  }
  Modelo <- try(
    SpATS(
      response = response,
      genotype = genotype,
      genotype.as.random = gen_ran,
      fixed = Fijo,
      spatial = ~ PSANOVA(
        col,
        row, 
        nseg = c(ncols, nrows), 
        degree = c(3, 3),
        nest.div = 2
      ),
      random = Random, 
      data = dt,
      control = list(tolerance = 1e-03, monitoring = 0)
    ), 
    silent = TRUE
  )
  if (class(Modelo) == "try-error"){
    stop(attr(Modelo, "condition"))
  } 
  if (class(Modelo) == "try-error") {
    return()
  }
  if (clean_out) {
    resum_out <- msa_residuals(Modelo, k_clean_out)
    if (resum_out > 0) {
      tmp_out <- 1
      counter <- 1
      while (tmp_out > 0 & counter <= iterations) {
        c_datos <- res_raw_data(Modelo, k_clean_out)
        c_datos[, response] <- ifelse(
          test = c_datos$Classify == "Outlier",
          yes = NA,
          no = c_datos[, response]
        )
        c_datos <- c_datos %>% dplyr::select(-weights)
        Modelo <- try(
          SpATS(
            response = response,
            genotype = genotype, genotype.as.random = gen_ran,
            fixed = Fijo,
            spatial = ~ PSANOVA(
              col,
              row,
              nseg = c(ncols, nrows),
              degree = c(3, 3),
              nest.div = 2
            ),
            random = Random,
            data = c_datos,
            control = list(tolerance = 1e-03, monitoring = 0)
          ), 
          silent = TRUE
        )
        tmp_out <- msa_residuals(Modelo, k_clean_out)
        if (iterations > 1) resum_out <- resum_out + tmp_out
        counter <- counter + 1
      }
    }
  }
  return(Modelo)  
}


check_spats <- function(data, response, genotype, experiment, col, row, two.stage = FALSE) {
  if (is.null(response) | response == "") {
    return()
  }
  if (is.null(genotype) | genotype == "") {
    return()
  }
  if (is.null(experiment) | experiment == "") {
    return()
  }

  data[, genotype] <- as.factor(data[, genotype])
  data[, experiment] <- as.factor(data[, experiment])

  if (isFALSE(two.stage)) {
    if (is.null(col) | col == "") {
      return()
    }
    if (is.null(row) | row == "") {
      return()
    }
    data[, col] <- as.factor(data[, col])
    data[, row] <- as.factor(data[, row])
    if (nlevels(data[, experiment]) <= 1) {
      shinytoastr::toastr_info(
        title = "Error:", "Only one level in the experiment factor.", position = "bottom-full-width",
        showMethod = "slideDown", hideMethod = "hide", hideEasing = "linear"
      )
      return()
    }

    dt <- data %>%
      dplyr::group_by(.data[[experiment]]) %>%
      dplyr::summarise(
        missing = sum(is.na(.data[[response]])),
        percentage = round(sum(is.na(.data[[response]])) / dplyr::n(), 3),
        ncol = dplyr::n_distinct(.data[[col]]),
        nrow = dplyr::n_distinct(.data[[row]]),
        ngen = dplyr::n_distinct(.data[[genotype]])
      )
    return(dt)
  } else {
    if (nlevels(data[, experiment]) <= 1) {
      return()
    }
    dt <- data %>%
      dplyr::group_by(.data[[experiment]]) %>%
      dplyr::summarise(
        missing = sum(is.na(.data[[response]])),
        percentage = round(sum(is.na(.data[[response]])) / dplyr::n(), 3),
        ngen = dplyr::n_distinct(.data[[genotype]])
      )
    return(dt)
  }
}

number_gen <- function(filter_data) {
  names(filter_data)[1] <- "Experiment"
  g0 <- filter_data %>%
    echarts4r::e_charts(Experiment) %>%
    echarts4r::e_bar(ngen, name = "Number of genotypes", bind = Experiment) %>%
    echarts4r::e_title("Number of genotypes", subtext = "by experiment") %>%
    echarts4r::e_tooltip() %>%
    echarts4r::e_legend(show = FALSE) %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage") %>%
    echarts4r::e_toolbox_feature(feature = "dataView") %>%
    echarts4r::e_labels() %>%
    echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 65, fontSize = 12, margin = 8)) %>% # rotate
    echarts4r::e_grid(height = "65%", top = "15%") %>%
    echarts4r::e_color("#28a745")
  g0
}

gen_share <- function(data = NULL, genotype = "line", exp = "Exp", response = NA) {
  data <- as.data.frame(data)
  nomb <- c(genotype, exp)
  if (sum(nomb %in% names(data)) != 2) {
    message("columns not found in the data")
    return()
  }
  data <- utils::type.convert(data, as.is = FALSE)
  data[, genotype] <- as.factor(data[, genotype])
  data[, exp] <- as.factor(data[, exp])
  if (!is.na(response)) data <- data[!is.na(data[, response]), ]
  nexp <- nlevels(data[, exp])
  ngen <- nlevels(data[, genotype])
  share <- matrix(NA, nrow = nexp, ncol = nexp)
  rownames(share) <- levels(data[, exp])
  colnames(share) <- levels(data[, exp])
  ind <- which(names(data) %in% exp)
  for (i in 1:nexp) {
    eitmp <- levels(droplevels(data[data[, ind] == colnames(share)[i], ])[, genotype])
    for (j in 1:nexp) {
      ejtmp <- levels(droplevels(data[data[, ind] == colnames(share)[j], ])[, genotype])
      share[i, j] <- sum(eitmp %in% ejtmp)
    }
  }
  return(share)
}

plot_shared <- function(share) {
  Share <- share %>%
    echarts4r::e_charts() %>%
    echarts4r::e_correlations(order = "hclust", visual_map = F) %>%
    echarts4r::e_tooltip() %>%
    echarts4r::e_visual_map(
      min = min(share),
      max = max(share),
      orient = "horizontal",
      left = "center",
      bottom = "bottom"
    ) %>%
    echarts4r::e_title("Shared genotypes", "By experiment") %>%
    echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = -45, fontSize = 12, margin = 8)) %>% # rotate
    echarts4r::e_grid(left = "20%", height = "60%")
  Share
}

dup_check <- function(data, experiment, column, row) {
  tt <- split(data, f = data[, experiment])
  lapply(tt, function(x) sum(duplicated(x[, c(column, row)])))
}

res_raw_data <- function(Model, k = 3) {
  dt <- Model$data
  VarE <- Model$psi[1]
  Data <- data.frame(Index = 1:length(stats::residuals(Model)), Residuals =stats::residuals(Model))
  u <- +k * sqrt(VarE)
  l <- -k * sqrt(VarE)
  Data$Classify <- NA
  Data$Classify[which(abs(Data$Residuals) >= u)] <- "Outlier"
  Data$Classify[which(abs(Data$Residuals) < u)] <- "Normal"

  dt$Classify <- Data$Classify
  return(dt)
}


# MSA

VarG_msa <- function(model) {
  gen <- model$model$geno$genotype
  gen_ran <- model$model$geno$as.random
  if (gen_ran) {
    vargen <- round(model$var.comp[gen], 3)
    names(vargen) <- "Var_Gen"
    return(vargen)
  } else {
    # pred <- predict(model,which = gen)[,"predicted.values"]
    # CV <- round(stats::sd(pred)/mean(pred),3)
    # names(CV) <- "CV"
    CV <- NA
    return(CV)
  }
}

VarE_msa <- function(model) {
  v <- round(model$psi[1], 3)
  names(v) <- "Var_Res"
  return(v)
}

h_msa <- function(model) {
  gen_ran <- model$model$geno$as.random
  if (gen_ran) {
    h <- getHeritability(model)
    return(h)
  } else {
    return(NA)
  }
}

msa_residuals <- function(model, k = 3) {
  value <- sum(res_data(model, k)$Classify == "Outlier", na.rm = T)
  return(value)
}

msa_table <- function(models, gen_ran, k = 3) {
  exp <- names(models)
  gv <- unlist(lapply(models, VarG_msa))
  ev <- unlist(lapply(models, VarE_msa))
  he <- unlist(lapply(models, h_msa))
  out <- unlist(lapply(models, msa_residuals, k))
  r2 <- unlist(lapply(models, R.square))
  cv <- unlist(lapply(models, CV.spats))
  summ <- data.frame(Experiment = exp, varG = gv, varE = ev, h2 = he, outliers = out, r2 = r2, cv = cv, row.names = NULL)
  return(summ)
}

msa_effects <- function(model) {
  gen <- model$model$geno$genotype

  ind <- sum(grepl("checks", model$model$fixed, fixed = TRUE)) != 0
  if (ind) {
    PP <- SpATS::predict.SpATS(model, which = gen, predFixed = "marginal") %>% dplyr::select(.data[[gen]], predicted.values, standard.errors)
    PP$type <- "test"
    PC <- SpATS::predict.SpATS(model, which = "checks", predFixed = "marginal") %>% dplyr::select(checks, predicted.values, standard.errors)
    PC <- PC[PC$checks != "_NoCheck", ]
    PC$type <- "check"
    names(PC)[1] <- gen
    effects <- rbind(PP, PC) %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      data.frame()
  } else {
    effects <- SpATS::predict.SpATS(model, which = gen, predFixed = "marginal")[, c(gen, "predicted.values", "standard.errors")] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      data.frame()
  }

  # effects <- predict(model, which = gen)[,c(gen, "predicted.values", "standard.errors")] %>%
  #            dplyr::mutate_if(is.numeric, round, 3) %>% data.frame()
  if (!model$model$geno$as.random) {
    effects <- weight_SpATS(model)$data_weights
  }
  return(effects)
}

multi_msa_effects <- function(models) {
  blups <- lapply(models, msa_effects)
  blups <- data.table::data.table(plyr::ldply(blups[], data.frame, .id = "Experiment"))
  return(blups)
}


table_outlier <- function(models, id = "trait", k = 3) {
  dt <- lapply(models, res_data, k)
  dt <- data.table::data.table(plyr::ldply(dt[], data.frame, .id = id))
  dt <- dt[dt$Classify == "Outlier", ] %>% dplyr::mutate_if(is.numeric, round, 3)
  return(dt)
}


# Weights

weight_SpATS <- function(model) {
  rand <- model$model$geno$as.random
  if (rand) {
    return()
  }

  C_inv <- as.matrix(rbind(
    cbind(model$vcov$C11_inv, model$vcov$C12_inv), # Combine components into one matrix C
    cbind(model$vcov$C21_inv, model$vcov$C22_inv)
  ))
  gen_mat <- colnames(model$vcov$C11_inv)

  genotype <- model$model$geno$genotype
  dt <- SpATS::predict.SpATS(model, which = genotype, predFixed = "marginal") %>%
    droplevels() %>%
    dplyr::mutate_if(is.numeric, round, 3)
  gen_lvls <- as.factor(unique(as.character(dt[, genotype])))

  intc <- intersect(gen_mat, gen_lvls)
  diff <- setdiff(gen_lvls, gen_mat)

  vcov <- C_inv[c("Intercept", intc), c("Intercept", intc)]
  colnames(vcov)[1] <- rownames(vcov)[1] <- diff
  diag_vcov <- diag(vcov)

  L <- diag(ncol(vcov))
  dimnames(L) <- list(colnames(vcov), rownames(vcov))
  L[, 1] <- 1
  Se2 <- diag(L %*% vcov %*% t(L))

  data_weights <- data.frame(gen = names(diag_vcov), vcov = diag_vcov, inv_vcov = 1 / diag_vcov, weights = 1 / Se2)
  data_weights <- merge(dt, data_weights, by.x = genotype, by.y = "gen", sort = F)
  data_weights <- data_weights[, c(genotype, "predicted.values", "standard.errors", "weights")] # "vcov","inv_vcov",

  return(list(
    vcov = vcov, diag = diag_vcov, diag_inv = 1 / diag_vcov,
    se2 = Se2, se = sqrt(Se2), data_weights = data_weights
  ))
}


#' @importFrom stats pchisq
Lik.ratio.test <- function(Model_nested, Model_full) {
  lo.lik1 <- Model_nested / -2
  lo.lik2 <- Model_full / -2

  d <- 2 * (lo.lik2 - lo.lik1)

  p.value1 <- round(1 - stats::pchisq(d, 1), 3)

  siglevel <- 0
  if (abs(p.value1) < 0.05) {
    siglevel <- "*"
  } else {
    siglevel <- "Not signif"
  }
  if (abs(p.value1) < 0.01) {
    siglevel <- "**"
  }
  if (abs(p.value1) < 0.001) {
    siglevel <- "***"
  }

  names(p.value1) <- "p-value"
  cat("\n========================================================")
  cat("\n", "Likelihood Ratio Test")
  cat("\n", "p-value =", p.value1, siglevel)
  cat("\n", "Sig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n")
  cat("========================================================")
}

varComp <- function(object, which = "variances") {
  which <- match.arg(which)
  var.comp <- object$var.comp
  psi <- object$psi[1]
  nterms <- length(var.comp)
  model <- names(var.comp)
  col.names <- c("Variance", "SD", "log10(lambda)")
  row.names <- c(model, NA, "Residual")
  vc <- matrix(ncol = 3, nrow = nterms + 2, dimnames = list(
    row.names,
    col.names
  ))
  vc[, 1] <- c(sprintf("%.3e", var.comp), NA, sprintf(
    "%.3e",
    psi
  ))
  vc[, 2] <- c(sprintf("%.3e", sqrt(var.comp)), NA, sprintf(
    "%.3e",
    sqrt(psi)
  ))
  vc[, 3] <- c(sprintf("%.5f", log10(psi / var.comp)), NA, NA)
  eff.dim <- object$eff.dim
  dim <- object$dim
  dim.nom <- object$dim.nom
  tot_ed <- sum(eff.dim, na.rm = TRUE)
  tot_dim <- sum(dim, na.rm = TRUE)
  dim.new <- dim[match(names(eff.dim), names(dim))]
  dim.nom <- dim.nom[match(names(eff.dim), names(dim.nom))]
  type <- rep(NA, length = length(dim.new))
  type[(attr(dim, "random") & !attr(dim, "spatial"))[match(
    names(eff.dim),
    names(dim)
  )]] <- "R"
  type[(!attr(dim, "random") & !attr(dim, "spatial"))[match(
    names(eff.dim),
    names(dim)
  )]] <- "F"
  type[is.na(type)] <- "S"
  eff.dim.new <- eff.dim
  smooth.comp <- attr(object$terms$spatial, "term")
  if (paste(smooth.comp, "Global") %in% names(dim)) {
    dim.new <- c(dim.new, dim[paste(smooth.comp, "Global")])
    dim.nom <- c(dim.nom, dim[paste(smooth.comp, "Global")])
    eff.dim.new <- c(eff.dim.new, sum(eff.dim.new[grep(smooth.comp,
      names(eff.dim.new),
      fixed = TRUE
    )]))
    names(eff.dim.new)[length(eff.dim.new)] <- paste(
      smooth.comp,
      "Global"
    )
    type <- c(type, "S")
  }
  ord <- c(which(type == "F"), which(type == "R"), which(type ==
    "S"))
  eff.dim.new <- eff.dim.new[ord]
  dim.new <- dim.new[ord]
  dim.nom <- dim.nom[ord]
  model <- model[ord]
  type <- type[ord]
  nterms <- length(eff.dim.new)
  model <- names(eff.dim.new)
  Nobs <- object$nobs
  col.names <- c(
    "Effective", "Model", "Nominal", "Ratio",
    "Type"
  )
  row.names <- c(model, NA, "Total", "Residual", "Nobs")
  m <- matrix(ncol = 5, nrow = nterms + 4, dimnames = list(
    row.names,
    col.names
  ))
  m[, 1] <- c(sprintf("%.1f", eff.dim.new), NA, sprintf(
    "%.1f",
    tot_ed
  ), sprintf("%.1f", Nobs - tot_ed), sprintf(
    "%.0f",
    Nobs
  ))
  m[, 2] <- c(sprintf("%.0f", dim.new), NA, sprintf(
    "%.0f",
    tot_dim
  ), NA, NA)
  m[, 3] <- c(sprintf("%.0f", dim.nom), NA, sprintf(
    "%.0f",
    sum(dim.nom, na.rm = TRUE)
  ), NA, NA)
  m[, 4] <- c(sprintf("%.2f", eff.dim.new / dim.nom), NA, sprintf(
    "%.2f",
    tot_ed / sum(dim.nom, na.rm = TRUE)
  ), NA, NA)
  m[, 5] <- c(type, NA, NA, NA, NA)
  object$p.table.vc <- vc
  object$p.table.dim <- m
  class(object) <- "summary.SpATS"

  v_name <- as.character(stats::na.omit(row.names(vc)))
  vc <- vc %>%
    as.data.frame() %>%
    utils::type.convert(as.is = FALSE) %>%
    dplyr::mutate_if(is.numeric, round, 3)

  vc <- vc[-c(nrow(vc) - 1), ]
  vc$Component <- v_name
  vc <- vc[, c(4, 1:3)]
  return(vc)
}

# SpATS coefficients

coef_SpATS <- function(model) {
  # coefficients
  coef_spats <- model$coeff
  coef_random <- attr(coef_spats, "random")
  coef_spats <- data.frame(level = names(coef_spats), solution = coef_spats, coef_random, row.names = NULL)

  # diagonal c_Inverse
  C_inv <- as.matrix(rbind(cbind(model$vcov$C11_inv, model$vcov$C12_inv), cbind(model$vcov$C21_inv, model$vcov$C22_inv)))
  se <- sqrt(diag(C_inv))
  C_inv <- data.frame(level = names(se), std.error = se, row.names = NULL)
  coef_spats <- merge(coef_spats, C_inv, by = "level", all = T)
  coef_spats <- coef_spats %>%
    dplyr::mutate(z.ratio = solution / std.error) %>%
    dplyr::mutate_if(is.numeric, round, 4) %>%
    dplyr::arrange(coef_random)
  coef_spats <- coef_spats[, c("level", "solution", "std.error", "z.ratio", "coef_random")]
  return(coef_spats)
}


# Daniel Ariza

ggCor <-
  function(myData, colours = c("#db4437", "white", "#FF9D00"),
           blackLabs = c(-0.7, 0.7), showSignif = TRUE,
           pBreaks = c(0, .001, .01, .05, Inf), pLabels = c("***", "**", "*", "ns"),
           showDiagonal = FALSE, Diag = NULL, returnTable = FALSE,
           size_text = 4) {

    #   Goal      : Return a ggplot object to plot a triangular correlation figure between 2 or more variables.
    #               Depends on the packages 'ggplot2' 'psych' and 'reshape'
    #
    #   Input     : myData       = A data.frame with numerical columns for each variable to be compared.
    #   Input     : colours      = A vector of size three with the colors to be used for values -1, 0 and 1.
    #   Input     : blackLabs    = A numeric vector of size two, with min and max correlation coefficient
    #                              limits to display with black tags. Any value outside this range will be
    #                              displayed with white tags.
    #   Input     : showSignif   = Logical scalar. Display significance values ?
    #   Input     : pBreaks      = Passed to function 'cut'. Either a numeric vector of two or more unique
    #                              cut points or a single number (greater than or equal to 2) giving the
    #                              number of intervals into which x is to be cut.
    #   Input     : pLabels      = Passed to function 'cut'. labels for the levels of the resulting category.
    #                              By default, labels are constructed using "(a,b]" interval notation.
    #                              If pLabels = FALSE, simple integer codes are returned instead of a factor.
    #   Input     : showDiagonal = Logical scalar. Display main diagonal values ?
    #   Input     : Diag         = A named vector of labels to display in the main diagonal. The names are
    #                              used to place each value in the corresponding coordinates of the diagonal.
    #                              Hence, these names must be the same as the colnames of myData
    #   Input     : returnTable  = Return the table to display instead of a ggplot object
    #
    #   Output    : A ggplot object containing a triangular correlation figure with all numeric variables
    #               in myData. If returnTable is TRUE, the table used to produce the figure is returned instead.
    #   Authors   : darizasu
    #    Last update: May 18, 2019


    # Drop non numeric columns in the dataset
    if (sum(!sapply(myData, is.numeric))) {
      message(
        "Dropping non-numeric columns in the dataset:\n",
        paste(names(which(!sapply(myData, is.numeric))),
          collapse = "\t"
        )
      )

      myData <- myData[, sapply(myData, is.numeric)]
    }

    # Calculate corr-coeffs and p values
    cors <- psych::corr.test(myData, use = "pairwise.complete.obs")

    # Use the adjusted p values for multiple testing instead of raw coeffs
    cors$p <- t(cors$p)

    # Keep only the matrices with correlation coefficients and p values
    cors <- cors[c(1, 4)]

    # For each matrix, do ...
    cors <- lapply(cors, function(x) {

      # Keep the upper triangle of the matrix
      x[upper.tri(x)] <- NA

      # Transpose the matrix to plot the lower triangle
      x <- as.data.frame(t(x))

      # Reshape the matrix to tidy format
      x[, "col"] <- colnames(x)
      x <- reshape::melt(x, id = "col")
      colnames(x) <- c("col", "row", "value")

      # Round coefficients
      x$name <- round(x$value, 2)

      # Sort the x axis according to myData column order
      x$col <- factor(x$col, levels = colnames(myData))

      # Reverse the y axis for a triangle plot from top-left to bottom-right
      x$row <- factor(x$row, levels = rev(colnames(myData)))

      # Remove NAs
      x <- stats::na.omit(x)
    })

    # Combine both dataframes with p values and corr coefficients
    cors <- merge(x = cors$r, y = cors$p, by = c("col", "row"))

    # Keep x, y, p val and corr-coefficients columns
    cors <- cors[, c(1, 2, 4, 5)]

    if (showSignif) {

      # Create a categorical variable for p values as defined by pBreaks
      cors$signi <- cut(
        x = cors$value.y, right = F,
        breaks = pBreaks, labels = pLabels
      )

      # Join corr-coeff and p-value to display it as a label for each tile
      cors$label <- paste(cors$name.x, cors$sign, sep = "\n")
    } else {

      # The label for each tile is the corr-coeff only
      cors$label <- cors$name.x
    }

    # If there are user-specified values to display in the diagonal
    if (!is.null(Diag)) {

      # Check the names in Diag are the same than colnames of myData
      if (sum(!names(Diag) %in% colnames(myData))) {
        warning(
          "These elements in 'Diag' do not correspond to column names in 'myData':\n",
          paste(names(Diag)[!names(Diag) %in% colnames(myData)],
            collapse = "\t"
          )
        )
      }

      # The tiles of the diagonal are gray
      cors[cors$col == cors$row, "name.x"] <- NA

      # Get the name of x and y levels
      d <- as.character(cors[cors$col == cors$row, "row"])

      # Modify the elements of the diagonal and make sure they are displayed
      cors[cors$col == cors$row, "label"] <- Diag[d]
      showDiagonal <- TRUE
    }

    # Remove the elements of the main diagonal if you don't want to display
    if (!showDiagonal) cors <- cors[cors$col != cors$row, ]

    # Show darker tiles with white labels for clarity
    cors$txtCol <- ifelse(cors$name.x > blackLabs[1] &
      cors$name.x < blackLabs[2], "black", "white")

    # Do not show tile labels for empty tiles.
    # Make tile labels of the diagonal white
    cors$txtCol[is.na(cors$txtCol)] <- "white"

    if (returnTable) {
      return(cors)
    }

    # require(ggplot2)

    p <- ggplot2::ggplot(data = cors, ggplot2::aes(x = col, y = row, fill = name.x)) +
      ggplot2::geom_tile(color = "gray") +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 16) +
      ggplot2::geom_text(ggplot2::aes(x = col, y = row, label = label), color = cors$txtCol, size = size_text) +
      ggplot2::scale_fill_gradient2(low = colours[1], mid = colours[2], high = colours[3]) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 40, hjust = 1), legend.position = "none",
        panel.grid.minor.x = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank()
      )

    return(p)
  }


checkConection <- function(data, genotype = "line", trial = "Experiment", response = "YDHA", all = FALSE) {
  if (all) {
    data %>%
      dplyr::filter(!is.na(.data[[response]])) %>%
      dplyr::select(.data[[genotype]], .data[[trial]]) %>%
      unique.data.frame() %>%
      dplyr::mutate(value = 1) %>%
      tidyr::spread(.data[[trial]], value = value) %>%
      dplyr::mutate(total = rowSums(dplyr::select(., -.data[[genotype]]), na.rm = T), n = ncol(.) - 1, percent = total / n) %>%
      dplyr::arrange(dplyr::desc(total))
  } else {
    data %>%
      dplyr::filter(!is.na(.data[[response]])) %>%
      dplyr::select(.data[[genotype]], .data[[trial]]) %>%
      unique.data.frame() %>%
      dplyr::mutate(value = 1) %>%
      tidyr::spread(.data[[trial]], value = value) %>%
      dplyr::mutate(total = rowSums(dplyr::select(., -.data[[genotype]]), na.rm = T), n = ncol(.) - 1, percent = total / n) %>%
      dplyr::arrange(dplyr::desc(total)) %>%
      dplyr::select(.data[[genotype]], total, n, percent)
  }
}


summ_complete <- function(data, grp = "", var) {
  if (is.numeric(data[, var])) {
    if (grp == "") {
      data %>%
        dplyr::summarise(
          N.Valid = rapportools::nvalid(.data[[var]], na.rm = TRUE),
          Pct.Valid = N.Valid * 100 / dplyr::n(),
          Mean = mean(.data[[var]], na.rm = TRUE),
          Std.Dev = stats::sd(.data[[var]], na.rm = TRUE),
          Min = min(.data[[var]], na.rm = TRUE),
          Q1 = stats::quantile(.data[[var]], probs = 0.25, type = 2, names = FALSE, na.rm = TRUE),
          Median = stats::median(.data[[var]], na.rm = TRUE),
          Q3 = stats::quantile(.data[[var]], probs = 0.75, type = 2, names = FALSE, na.rm = TRUE),
          Max = max(.data[[var]], na.rm = TRUE),
          MAD = stats::mad(.data[[var]], na.rm = TRUE),
          IQR = stats::IQR(.data[[var]], na.rm = TRUE),
          CV = Std.Dev / Mean,
          Skewness = rapportools::skewness(.data[[var]], na.rm = TRUE),
          SE.Skewness = sqrt((6 * N.Valid * (N.Valid - 1)) / ((N.Valid - 2) * (N.Valid + 1) * (N.Valid + 3))),
          Kurtosis = rapportools::kurtosis(.data[[var]], na.rm = TRUE)
        )
    } else {
      data %>%
        dplyr::group_by(.data[[grp]]) %>%
        dplyr::summarise(
          N.Valid = rapportools::nvalid(.data[[var]], na.rm = TRUE),
          Pct.Valid = N.Valid * 100 / dplyr::n(),
          Mean = mean(.data[[var]], na.rm = TRUE),
          Std.Dev = stats::sd(.data[[var]], na.rm = TRUE),
          Min = min(.data[[var]], na.rm = TRUE),
          Q1 = stats::quantile(.data[[var]], probs = 0.25, type = 2, names = FALSE, na.rm = TRUE),
          Median = stats::median(.data[[var]], na.rm = TRUE),
          Q3 = stats::quantile(.data[[var]], probs = 0.75, type = 2, names = FALSE, na.rm = TRUE),
          Max = max(.data[[var]], na.rm = TRUE),
          MAD = stats::mad(.data[[var]], na.rm = TRUE),
          IQR = stats::IQR(.data[[var]], na.rm = TRUE),
          CV = Std.Dev / Mean,
          Skewness = rapportools::skewness(.data[[var]], na.rm = TRUE),
          SE.Skewness = sqrt((6 * N.Valid * (N.Valid - 1)) / ((N.Valid - 2) * (N.Valid + 1) * (N.Valid + 3))),
          Kurtosis = rapportools::kurtosis(.data[[var]], na.rm = TRUE)
        )
    }
  } else {
    if (grp == "") {
      data %>%
        dplyr::summarise(
          Variable = var,
          N.Valid = rapportools::nvalid(.data[[var]], na.rm = TRUE),
          Pct.Valid = N.Valid * 100 / dplyr::n()
        )
    } else {
      data %>%
        dplyr::group_by(.data[[grp]]) %>%
        dplyr::summarise(
          Variable = var,
          N.Valid = rapportools::nvalid(.data[[var]], na.rm = TRUE),
          Pct.Valid = N.Valid * 100 / dplyr::n()
        )
    }
  }
}
