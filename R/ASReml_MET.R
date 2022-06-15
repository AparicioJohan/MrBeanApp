#' Routine to perform final step of a two-stage MET analyses
#'
#' \code{stageMET} Performs the genetic analysis of an MET dataset corresponding
#' to the final step of a two-stage analysis, where input corresponds to meand (or
#' adjusted values) of Evaluates and verifies the data originating from several trials with the
#' aim of determining levels of: connectivity, variability, etc., which are reported as
#' statistics. Only non-NA observations are considered in the reports. Note that trial is
#' always considered a fixed effect.
#
#' @param data dataframe with all relevant columns for MET analyses.
#' @param gen factor name for genotypes (or treatments)
#' @param trial factor name for trial (or environment)
#' @param resp column name for the response variable to evaluate
#' @param weight column name for the weight of response (default = 1)
#' @param type.gen model assumption for genotype effects: 'random' or 'fixed' (default = 'random')
#' @param type.trial model assumption for trial effects: 'random' or 'fixed' (default = 'fixed')
#' @param vc.model variance-covariance model to fit: 'diag', 'corv', 'corh', 'fa1', 'fa2',
#'                 'fa3', 'fa4', 'corgh' (default = 'corh') (only for type.gen =' random')
#' @param workspace asreml workspace
#' @return Several objects with reports of the MET analysis.
#' call:        String with the ASReml-R call used to fit the requested model
#' mod:         ASReml-R object with all information from the fitted model
#' predictions: Predictions for all genotypes across all sites, with thier standard
#'               error and reliability.
#'
#' @author
#' Salvador A. Gezan. VSN International
#'
#' @examples
#' # Example 1:
#'
stageMET <- function(data = NULL, gen = NULL, trial = NULL, resp = NULL, weight = NULL,
                     type.gen = "random", type.trial = "fixed", vc.model = "corh", workspace = "128mb") {
  asreml::asreml.options(trace = FALSE, workspace = workspace)
  n <- nrow(data)
  if (n == 0) {
    stop("No information in data.frame provided.")
  }
  # Defining factors
  df <- data.frame(IDSORT = c(1:n))
  if (is.null(gen)) {
    stop("No genotype column provided.")
  } else {
    df$gen <- as.factor(data[, gen])
  }
  if (is.null(trial)) {
    stop("No trial column provided.")
  } else {
    df$trial <- as.factor(data[, trial])
    s <- length(levels(df$trial))
    if (s <= 1) {
      stop("Only 1 trial on the data, this is not an MET.")
    }
  }
  if (is.null(resp)) {
    stop("No response column provided.")
  } else {
    df$resp <- data[, resp]
  }
  if (is.null(weight)) {
    message("No weight column provided, all weights = 1.")
    df$weight <- NA
    df$weight[!is.na(df$resp)] <- 1
    if (sum(is.na(df$weight)) > 0) { # length(is.na(df$weight))>0
      message("Weight column has some missing values, respective records were eliminated.")
      # Eliminating NA on weights
      df <- df[!is.na(df$weight), ]
    }
  } else {
    df$weight <- data[, weight]
    if (sum(is.na(df$weight)) > 0) { # length(is.na(df$weight))>0
      message("Weight column has some missing values, respective records were eliminated.")
      # Eliminating NA on weights
      df <- df[!is.na(df$weight), ]
    }
  }

  # df: IDSORT, gen, trial, resp, weight
  # Code Strings for ASReml-R
  code.asr <- as.character()
  code.asr[1] <- "asreml::asreml(fixed=resp~1"
  code.asr[2] <- "random=~"
  code.asr[3] <- "weights=weight"
  code.asr[4] <- "family=asreml::asr_gaussian(dispersion=1)"
  code.asr[5] <- 'na.action=list(x="include",y="include"),data=df)' # workspace=128e06
  nrand <- 0 # Number of random terms

  # Adding gen (fixed or random)
  if (type.gen == "fixed") {
    if (type.trial == "fixed") {
      code.asr[1] <- paste(code.asr[1], "gen+trial+trial:gen", sep = "+")
    }
    if (type.trial == "random") {
      code.asr[1] <- paste(code.asr[1], "gen", sep = "+")
      code.asr[2] <- paste(code.asr[2], "trial+trial:gen", sep = "+")
      nrand <- nrand + 1
    }
  }
  # gen random
  if (type.gen == "random") {
    nrand <- nrand + 1
    if (type.trial == "fixed") {
      code.asr[1] <- paste(code.asr[1], "trial", sep = "+")
    }
    if (type.trial == "random") {
      code.asr[2] <- paste(code.asr[2], "trial", sep = "+")
    }
    if (vc.model == "diag") {
      code.asr[2] <- paste(code.asr[2], "diag(trial):id(gen)", sep = "+")
    }
    if (vc.model == "corv") {
      code.asr[2] <- paste(code.asr[2], "corv(trial):id(gen)", sep = "+")
    }
    if (vc.model == "corh") {
      code.asr[2] <- paste(code.asr[2], "corh(trial):id(gen)", sep = "+")
    }
    if (vc.model == "fa1") {
      if (s <= 2) {
        message("This Factor Analytic 1 analysis is over-parametrized.")
      }
      code.asr[2] <- paste(code.asr[2], "fa(trial,1):gen", sep = "+") # id(gen)
    }
    if (vc.model == "fa2") {
      if (s <= 4) {
        message("This Factor Analytic 2 analysis is over-parametrized.")
      }
      code.asr[2] <- paste(code.asr[2], "fa(trial,2):gen", sep = "+")
    }
    if (vc.model == "fa3") {
      if (s <= 6) {
        message("This Factor Analytic 3 analysis is over-parametrized.")
      }
      code.asr[2] <- paste(code.asr[2], "fa(trial,3):gen", sep = "+")
    }
    if (vc.model == "fa4") {
      if (s <= 8) {
        message("This Factor Analytic 4 analysis is over-parametrized.")
      }
      code.asr[2] <- paste(code.asr[2], "fa(trial,4):gen", sep = "+")
    }
    if (vc.model == "corgh") {
      code.asr[2] <- paste(code.asr[2], "corgh(trial):id(gen)", sep = "+")
    }
  }

  # Running final MET models in ASReml-R
  code.asr[1] <- paste("mod.ref<-", code.asr[1], sep = "")
  if (nrand == 0) {
    str.mod <- paste(code.asr[1], code.asr[3], code.asr[4], code.asr[5], sep = ",")
  }
  if (nrand != 0) {
    str.mod <- paste(code.asr[1], code.asr[2], code.asr[3], code.asr[4], code.asr[5], sep = ",")
  }
  # print(str.mod)
  eval(parse(text = str.mod))
  if (!mod.ref$converge) {
    eval(parse(text = "mod.ref<-asreml::update.asreml(mod.ref)"))
  }

  # Obtaining predictions for models
  pvals <- predict(mod.ref, classify = "trial:gen", sed = FALSE, vcov = FALSE)$pvals # pworkspace=1e08
  pvals2 <- predict(mod.ref, classify = "gen", sed = FALSE, vcov = FALSE)$pvals
  # Obtaining some statistics
  gfit <- matrix(NA, ncol = 4, nrow = 1)
  gfit[1] <- nrow(summary(mod.ref)$varcomp)
  gfit[2] <- summary(mod.ref)$loglik
  gfit[3] <- summary(mod.ref)$aic
  gfit[4] <- summary(mod.ref)$bic
  colnames(gfit) <- c("n.VC", "logL", "AIC", "BIC")

  # Extracting Variance-Covariance\Correlation Matrix
  if (type.gen == "fixed") {
    VCOV <- NA
    CORR <- NA
  }
  if (type.gen == "random") {
    vc <- summary(mod.ref)$varcomp
    VCOV <- matrix(0, ncol = s, nrow = s)
    CORR <- matrix(0, ncol = s, nrow = s)
    diag(CORR) <- rep(1, s)
    if (vc.model == "diag") {
      vc <- vc[grep("trial:gen", rownames(vc)), ]
      diag(VCOV) <- vc[, 1]
    }
    if (vc.model == "corv") {
      vc <- vc[grep("trial:gen", rownames(vc)), ]
      CORR <- matrix(1, ncol = s, nrow = s)
      CORR <- vc[1, 1] * CORR
      diag(CORR) <- rep(1, s)
      D <- rep(vc[2, 1], s)
      VCOV <- diag(sqrt(D)) %*% CORR %*% diag(sqrt(D))
    }
    if (vc.model == "corh") {
      vc <- vc[grep("trial:gen", rownames(vc)), ]
      CORR <- matrix(1, ncol = s, nrow = s)
      CORR <- vc[1, 1] * CORR
      diag(CORR) <- rep(1, s)
      D <- vc[2:(s + 1), 1]
      VCOV <- diag(sqrt(D)) %*% CORR %*% diag(sqrt(D))
    }
    if (vc.model == "fa1") {
      vc.var <- vc[grep("!var", rownames(vc)), ]
      vc.fa1 <- vc[grep("!fa1", rownames(vc)), ]
      R <- vc.var[, 1]
      L <- vc.fa1[, 1]
      VCOV <- L %*% t(L) + diag(R)
      CORR <- cov2cor(VCOV)
    }
    if (vc.model == "fa2") {
      vc.var <- vc[grep("!var", rownames(vc)), ]
      vc.fa1 <- vc[grep("!fa1", rownames(vc)), ]
      vc.fa2 <- vc[grep("!fa2", rownames(vc)), ]
      R <- vc.var[, 1]
      L1 <- vc.fa1[, 1]
      L2 <- vc.fa2[, 1]
      L <- cbind(L1, L2)
      VCOV <- L %*% t(L) + diag(R)
      CORR <- cov2cor(VCOV)
    }
    if (vc.model == "fa3") {
      vc.var <- vc[grep("!var", rownames(vc)), ]
      vc.fa1 <- vc[grep("!fa1", rownames(vc)), ]
      vc.fa2 <- vc[grep("!fa2", rownames(vc)), ]
      vc.fa3 <- vc[grep("!fa3", rownames(vc)), ]
      R <- vc.var[, 1]
      L1 <- vc.fa1[, 1]
      L2 <- vc.fa2[, 1]
      L3 <- vc.fa3[, 1]
      L <- cbind(L1, L2, L3)
      VCOV <- L %*% t(L) + diag(R)
      CORR <- cov2cor(VCOV)
    }
    if (vc.model == "fa4") {
      vc.var <- vc[grep("!var", rownames(vc)), ]
      vc.fa1 <- vc[grep("!fa1", rownames(vc)), ]
      vc.fa2 <- vc[grep("!fa2", rownames(vc)), ]
      vc.fa3 <- vc[grep("!fa3", rownames(vc)), ]
      vc.fa4 <- vc[grep("!fa4", rownames(vc)), ]
      R <- vc.var[, 1]
      L1 <- vc.fa1[, 1]
      L2 <- vc.fa2[, 1]
      L3 <- vc.fa3[, 1]
      L4 <- vc.fa4[, 1]
      L <- cbind(L1, L2, L3, L4)
      VCOV <- L %*% t(L) + diag(R)
      CORR <- cov2cor(VCOV)
    }
    if (vc.model == "corgh") {
      vc.corr <- vc[grep(".cor", rownames(vc)), ]
      vc.var <- vc[-grep(".cor", rownames(vc)), ]
      k <- 1
      for (i in 1:s) {
        for (j in 1:i) {
          if (i != j) {
            CORR[i, j] <- vc.corr[k, 1]
            CORR[j, i] <- vc.corr[k, 1]
            k <- k + 1
          }
        }
      }
      D <- vc.var[1:s, 1]
      VCOV <- diag(sqrt(D)) %*% CORR %*% diag(sqrt(D))
    }
    colnames(VCOV) <- levels(df$trial)
    colnames(CORR) <- levels(df$trial)
    rownames(VCOV) <- levels(df$trial)
    rownames(CORR) <- levels(df$trial)
  }

  return(list(
    call = str.mod, mod = mod.ref, predictions = pvals,
    gfit = gfit, vcovM = VCOV, corrM = CORR, overall = pvals2
  ))
}



covariance_asreml <- function(matrix, corr = TRUE, size = 4) {

  # Get lower triangle of the correlation matrix
  get_lower_tri <- function(cormat) {
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat) {
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }

  reorder_cormat <- function(cormat) {
    # Use correlation between variables as distance
    dd <- as.dist((1 - cormat) / 2)
    hc <- hclust(dd)
    cormat <- cormat[hc$order, hc$order]
  }

  cormat <- reorder_cormat(matrix)
  upper_tri <- get_upper_tri(matrix)
  melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

  u <- -1
  m <- 0
  l <- 1
  main <- "Correlation"
  col_pallete <- c("#db4437", "white", "#4285f4")
  col_letter <- "black"

  if (isFALSE(corr)) {
    u <- min(matrix, na.rm = T)
    l <- max(matrix, na.rm = T)
    m <- u + (l - u) / 2
    main <- "Covariance"
    col_pallete <- c("#440154", "#21908C", "#FDE725")
    col_letter <- "white"
  }

  melted_cormat$Var1 <- as.factor(melted_cormat$Var1)
  melted_cormat$Var2 <- as.factor(melted_cormat$Var2)

  ggheatmap <- ggplot2::ggplot(melted_cormat, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(
      low = col_pallete[1], high = col_pallete[3], mid = col_pallete[2], # color= c("#440154","#21908C","#FDE725")
      midpoint = m, limit = c(u, l), space = "Lab",
      name = main
    ) +
    ggplot2::theme_minimal() + # minimal theme
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45, vjust = 1,
        size = 12, hjust = 1
      ),
      axis.text.y = ggplot2::element_text(size = 12)
    )
  # coord_fixed()


  plot <- ggheatmap +
    ggplot2::geom_text(ggplot2::aes(Var2, Var1, label = value), color = col_letter, size = size) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal"
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(
      barwidth = 7, barheight = 1,
      title.position = "top", title.hjust = 0.5
    ))

  return(plot)
}




test_lrt_2stage <- function(m0 = NULL, m1 = NULL) {
  if (is.null(m0)) {
    return()
  }
  if (is.null(m1)) {
    return()
  }

  likelihood <- try(asreml::lrt.asreml(m0$mod, m1$mod, boundary = FALSE), silent = T)

  return(likelihood)
}
