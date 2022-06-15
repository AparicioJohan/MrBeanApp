#' Routine to obtain several goodness-of-fit statistics for spatial and non-spatial models
#'
#' \code{stats.spatial} Internal routine that calculates goodness-of-fit statisitcs for a
#' specific fitted model provided as input. Statistics obtained are log-likelihood, AIC,
#' BIC, heritability based on predictor error variance (heritPEV, only for genotypes 'random'),
#' A-optimality value, and logarithm of D-optimality value,
#'
#' @param object model fit output for object class asreml. Genotype factor is called 'gen'
#' @param checks logical to indicate if checks were included in the model (default = FALSE)
#' @param solution data frame with the random solutions from asreml (BLUPs)
#' @param preds data frame with the predictions for genotypes (required to calculate Aopt, Dopt)
#' @param k value 1 or 2 to be used for the denominator of heritabilty calculation (herit.PEV) (default = 2)
#' @param ctable table with genotypes and checks (optional)
#'
#' @return A table with goodness-of-fit statistics for models evaluated. This includes columns:
#' number of variance components in the model (n.VC), log-likelihood (logL), Akaike information
#' criteria (AIC), Bayesian information criteria (BIC), heritability based on variance components,
#' heritability based on predictor error variance (heritPEV, only for genotypes 'random'), A-optimality value, and logarithm of
#' D-optimality value.
#'
#' @author
#' Salvador A. Gezan. VSN International
#'
#' @examples
#' # Example: Pending
#'
stats.spatial <- function(object = NULL, checks = FALSE, solution = NULL, preds = NULL, k = 1, ctable = NULL) {
  if (is.null(object)) {
    stop("No fitted model provided.")
  }
  ss <- summary(object)
  vc <- summary(object)$varcomp

  # Extracting the solutions and calculating H2PEV and H2VC
  if (is.null(solution)) {
    H2PEV <- NA
    H2VC <- NA
  }
  if (!is.null(solution)) {
    sol.gen <- as.data.frame(solution[grep("gen", rownames(solution)), ])
    var.gen <- vc[grep("gen", rownames(vc)), 1]
    H2PEV <- 1 - mean(sol.gen$std.error^2) / (k * var.gen)
    vctemp <- vc[vc$bound != "F" & vc$bound != "U", 1]
    H2VC <- var.gen / sum(vctemp)
  }

  # Dealing with the predictions: Aopt / Dopt
  # Note that these can be restricted to only comparisons against controls
  if (!is.null(preds)) {
    if (!checks) {
      pvals <- preds$pvals
      vcov <- as.matrix(preds$vcov)
      sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
      sel[is.na(pvals$predicted.value), ] <- 0
      vcov <- vcov[sel == 1, sel == 1]
      Aopt <- mean(diag(vcov))
      Dopt <- log(det(vcov))
    }
    # This might need some work as it is done on all
    if (checks) {
      pvals <- preds$pvals
      vcov <- as.matrix(preds$vcov)
      sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
      sel[is.na(pvals$predicted.value), ] <- 0
      vcov <- vcov[sel == 1, sel == 1]
      Aopt <- mean(diag(vcov))
      Dopt <- log(det(vcov))
    }
  }
  if (is.null(preds)) {
    Aopt <- NA
    Dopt <- NA
  }

  return(list(
    n.vc = nrow(vc), logL = ss$loglik, aic = ss$aic, bic = ss$bic,
    Aopt = Aopt, Dopt = Dopt, herit.PEV = H2PEV, herit.VC = H2VC
  ))
}


conv_null <- function(x) {
  if (x == "") {
    NULL
  } else {
    x
  }
}



#' Routine to fit several spatial models for Augmented, Unreplicated and P-Rep Designs
#'
#' \code{spatial.aud} Generates a table with the goodness-of-fit statisitcs to select the
#' best spatial model for an analysis of an Augmented, Unreplicated or a P-Rep design.
#' The models fitted are: 1) independent errors, 2) spatial (ar1) in columns, 3) spatial (ar1)
#' in rows, and 4) spatial (ar1) in both rows and columns. If blocks are provided, these will
#' be fitted as random effects. Genotype (or treatment) effects can be specified as random or fixed.
#' If an additional factor of checks is provided then these will be fitted as fixed effects.
#'
#' @param data dataframe with all relevant columns for spatial model and response variables.
#' @param gen factor name for genotypes (or treatments)
#' @param check factor name for checks (labeled 'control') and test lines (labeled 'test') (optional)
#' @param block factor name for block (or replicates) (optional)
#' @param row factor name for row coordinates of each experimental unit
#' @param col factor name for column coordinates of each experimental unit
#' @param covariate factor for additional covariate (optional)
#' @param nugget logical to fit nugget random effects (default = FALSE)
#' @param resp column name for the response variable to analyze
#' @param type.gen model assumption for genotypes: 'random' or 'fixed' (default = 'random')
#' @param model model number to be fitted (optional)
#'
#' @return A table with goodness-of-fit statistics for models evaluated. This includes columns:
#' number of variance components in the model (n.VC), log-likelihood (logL), Akaike information
#' criteria (AIC), Bayesian information criteria (BIC), heritability based on predictor
#' error variance (heritPEV, only for genotypes 'random'), A-optimality value, logarithm of
#' D-optimality value, and pvalue of comparison of each model against Independent Model based
#' on a likelihood ratio test (LRT).
#' If parameter model is specified an object with fitted model number is provided, together with
#' predictions for genotypes, here an additional column of 'weight' is incorporated to be used
#' on a second-stage analysis. These weights are the diagonal of the inverse of the variance-
#' covariance matrix of predictions.
#'
#' @author
#' Salvador A. Gezan. VSN International
#'
#' @examples
#' # Example 1: Analysis Unreplicated Trial
#' # library(agridat)
#' # UR <- burgueno.unreplicated
#' # UR$col <- as.factor(UR$col)
#' # UR$row <- as.factor(UR$row)
#' # UR$gen <- as.factor(UR$gen)
#' # head(UR)
#' # output.UR <- spatial.aud(data=UR, gen='gen', row='row', col='col', resp='yield',
#' #                          type.gen='random')
#' # output.UR
#' #
#' # # Example 2: Fitting only Model 4: ar1(Row):ar1(Col)
#' # mod4.UR <- spatial.aud(data=UR, gen='gen', row='row', col='col', resp='yield',
#' #                        type.gen='random', model=4)
#' # summary(mod4.UR$mod)$varcomp  # Summary from ASReml-R
#' # head(mod4.UR$predictions)     # Predictions
#'
spatial.aud <- function(data = NULL, gen = NULL, check = NULL, block = NULL, row = NULL, col = NULL,
                        covariate = NULL, nugget = FALSE, resp = NULL,
                        type.gen = "random", model = NULL) {
  asreml::asreml.options(trace = FALSE)
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
  if (is.null(row) || is.null(col)) {
    warning("No spatial coordinates provided.")
  } else {
    df$row <- as.factor(data[, row])
    df$col <- as.factor(data[, col])
    nr <- length(unique(df$row))
    nc <- length(unique(df$col))
    if (n != nr * nc) {
      stop("Number of observations does not conform with #rows x #columns.")
    }
    # df<-df[order(df$row,df$col),]  # order data by row then column
  }
  if (!is.null(check)) {
    if (type.gen == "fixed") {
      warning("Both control and test genotypes are fixed: genotypes were not separated.")
      check <- NULL
    }
    if (!is.null(check)) {
      df$check <- as.factor(data[, check])
      ncheck <- length(unique(df$check))
      if (ncheck != 2) {
        stop("Number of levels in check should be 2: control / test")
      }
      lev_check <- unique(df$check)
      cont <- 0
      test <- 0
      if (lev_check[1] == "control" | lev_check[2] == "control") {
        cont <- 1
      }
      if (lev_check[1] == "test" | lev_check[2] == "test") {
        test <- 1
      }
      if (cont + test != 2) {
        stop("Your levels for checks are not words: control / test")
      }
    }
  }
  if (!is.null(block)) {
    df$block <- as.factor(data[, block])
  }
  if (!is.null(covariate)) {
    df$cov <- as.numeric(data[, covariate])
  }
  if (is.null(resp)) {
    stop("No response variable indicated.")
  } else {
    df$resp <- data[, resp]
  }

  # Code Strings for ASReml-R
  code.asr <- as.character()
  code.asr[1] <- "asreml::asreml(fixed=resp~1"
  code.asr[2] <- "random=~"
  code.asr[3] <- "residual=~"
  code.asr[4] <- 'na.action=list(x="include",y="include"),data=df)'
  nrand <- 0 # Number of random terms
  # mod.sp<-paste(code.asr[1],code.asr[2],code.asr[3],code.asr[4],sep=',')
  # mod.sp

  # Adding covariate (fixed)
  if (!is.null(covariate)) {
    code.asr[1] <- paste(code.asr[1], "cov", sep = "+")
  }
  # Adding blocks (random)
  if (!is.null(block)) { #
    code.asr[2] <- paste(code.asr[2], "block", sep = "+")
    nrand <- nrand + 1
  }
  # All gen (control+test fixed)
  if (type.gen == "fixed") {
    code.asr[1] <- paste(code.asr[1], "gen", sep = "+")
  }
  # Genotype random
  if (type.gen == "random") {
    # No separation with checks
    if (is.null(check)) {
      code.asr[2] <- paste(code.asr[2], "gen", sep = "+")
      nrand <- nrand + 1
    }
    # Checks as fixed effects
    if (!is.null(check)) {
      code.asr[1] <- paste(code.asr[1], "at(check,'control'):gen", sep = "+")
      code.asr[2] <- paste(code.asr[2], "at(check,'test'):gen", sep = "+")
      nrand <- nrand + 1
    }
  }

  df <- df[order(df$row, df$col), ] # order data by row then column
  # Running final spatial models in ASReml-R
  code.t <- code.asr[1]
  # mod.ii
  code.asr[3] <- "residual=~id(row):id(col)"
  code.asr[1] <- paste("mod.ii<-", code.t, sep = "")
  if (nrand == 0) {
    str.mod.ii <- paste(code.asr[1], code.asr[3], code.asr[4], sep = ",")
  }
  if (nrand != 0) {
    str.mod.ii <- paste(code.asr[1], code.asr[2], code.asr[3], code.asr[4], sep = ",")
  }

  # Adding nugget (random) only to spatial models
  if (nugget) {
    code.asr[2] <- paste(code.asr[2], "units", sep = "+")
    nramd <- nrand + 1
  }

  # mod.ic
  code.asr[3] <- "residual=~id(row):ar1(col)"
  code.asr[1] <- paste("mod.ic<-", code.t, sep = "")
  if (nrand == 0) {
    str.mod.ic <- paste(code.asr[1], code.asr[3], code.asr[4], sep = ",")
  }
  if (nrand != 0) {
    str.mod.ic <- paste(code.asr[1], code.asr[2], code.asr[3], code.asr[4], sep = ",")
  }
  # mod.ri
  code.asr[3] <- "residual=~ar1(row):id(col)"
  code.asr[1] <- paste("mod.ri<-", code.t, sep = "")
  if (nrand == 0) {
    str.mod.ri <- paste(code.asr[1], code.asr[3], code.asr[4], sep = ",")
  }
  if (nrand != 0) {
    str.mod.ri <- paste(code.asr[1], code.asr[2], code.asr[3], code.asr[4], sep = ",")
  }
  # mod.rc
  code.asr[3] <- "residual=~ar1(row):ar1(col)"
  code.asr[1] <- paste("mod.rc<-", code.t, sep = "")
  if (nrand == 0) {
    str.mod.rc <- paste(code.asr[1], code.asr[3], code.asr[4], sep = ",")
  }
  if (nrand != 0) {
    str.mod.rc <- paste(code.asr[1], code.asr[2], code.asr[3], code.asr[4], sep = ",")
  }

  if (is.null(model)) {
    eval(parse(text = str.mod.ii))
    if (!mod.ii$converge) {
      eval(parse(text = "mod.ii<-asreml::update.asreml(mod.ii)"))
    }
    eval(parse(text = str.mod.ic))
    if (!mod.ic$converge) {
      eval(parse(text = "mod.ic<-asreml::update.asreml(mod.ic)"))
    }
    eval(parse(text = str.mod.ri))
    if (!mod.ri$converge) {
      eval(parse(text = "mod.ri<-asreml::update.asreml(mod.ri)"))
    }
    eval(parse(text = str.mod.rc))
    if (!mod.rc$converge) {
      eval(parse(text = "mod.rc<-asreml::update.asreml(mod.rc)"))
    }

    # Obtaining solutions/predictions for models
    if (type.gen == "fixed") {
      sol.ii <- NULL
      sol.ic <- NULL
      sol.ri <- NULL
      sol.rc <- NULL
      pvals.ii <- predict(mod.ii, classify = "gen", vcov = TRUE)
      pvals.ic <- predict(mod.ic, classify = "gen", vcov = TRUE)
      pvals.ri <- predict(mod.ri, classify = "gen", vcov = TRUE)
      pvals.rc <- predict(mod.rc, classify = "gen", vcov = TRUE)
    }
    if (type.gen == "random") {
      sol.ii <- summary(mod.ii, coef = TRUE)$coef.random
      sol.ic <- summary(mod.ic, coef = TRUE)$coef.random
      sol.ri <- summary(mod.ri, coef = TRUE)$coef.random
      sol.rc <- summary(mod.rc, coef = TRUE)$coef.random
      if (is.null(check)) { # No separation of checks
        pvals.ii <- predict(mod.ii, classify = "gen", vcov = TRUE)
        pvals.ic <- predict(mod.ic, classify = "gen", vcov = TRUE)
        pvals.ri <- predict(mod.ri, classify = "gen", vcov = TRUE)
        pvals.rc <- predict(mod.rc, classify = "gen", vcov = TRUE)
      }
      if (!is.null(check)) { # With separation of checks
        pvals.ii <- predict(mod.ii, classify = "check:gen", vcov = TRUE)
        pvals.ic <- predict(mod.ic, classify = "check:gen", vcov = TRUE)
        pvals.ri <- predict(mod.ri, classify = "check:gen", vcov = TRUE)
        pvals.rc <- predict(mod.rc, classify = "check:gen", vcov = TRUE)
      }
    }

    # Table of data for genotypes vs checks
    # ctable <- data.frame(table(df$gen,df$check))

    # Calling the statistics
    if (is.null(check)) {
      checks <- FALSE
      ctable <- data.frame(table(df$gen, 1:length(df$gen)))
    }
    if (!is.null(check)) {
      checks <- TRUE
      ctable <- data.frame(table(df$gen, df$check))
    }
    gfit <- matrix(NA, ncol = 9, nrow = 5)
    gt <- stats.spatial(object = mod.ii, checks = checks, solution = sol.ii, preds = pvals.ii, ctable = ctable)
    gfit[1, 2] <- gt$n.vc
    gfit[1, 3] <- gt$logL
    gfit[1, 4] <- gt$aic
    gfit[1, 5] <- gt$bic
    gfit[1, 6] <- gt$herit.PEV
    gfit[1, 7] <- gt$herit.VC
    gfit[1, 8] <- gt$Aopt
    gfit[1, 9] <- gt$Dopt
    gt <- stats.spatial(object = mod.ic, checks = checks, solution = sol.ic, preds = pvals.ic, ctable = ctable)
    gfit[2, 2] <- gt$n.vc
    gfit[2, 3] <- gt$logL
    gfit[2, 4] <- gt$aic
    gfit[2, 5] <- gt$bic
    gfit[2, 6] <- gt$herit.PEV
    gfit[2, 7] <- gt$herit.VC
    gfit[2, 8] <- gt$Aopt
    gfit[2, 9] <- gt$Dopt
    gt <- stats.spatial(object = mod.ri, checks = checks, solution = sol.ri, preds = pvals.ri, ctable = ctable)
    gfit[3, 2] <- gt$n.vc
    gfit[3, 3] <- gt$logL
    gfit[3, 4] <- gt$aic
    gfit[3, 5] <- gt$bic
    gfit[3, 6] <- gt$herit.PEV
    gfit[3, 7] <- gt$herit.VC
    gfit[3, 8] <- gt$Aopt
    gfit[3, 9] <- gt$Dopt
    gt <- stats.spatial(object = mod.rc, checks = checks, solution = sol.rc, preds = pvals.rc, ctable = ctable)
    gfit[4, 2] <- gt$n.vc
    gfit[4, 3] <- gt$logL
    gfit[4, 4] <- gt$aic
    gfit[4, 5] <- gt$bic
    gfit[4, 6] <- gt$herit.PEV
    gfit[4, 7] <- gt$herit.VC
    gfit[4, 8] <- gt$Aopt
    gfit[4, 9] <- gt$Dopt
    colnames(gfit) <- c("MODEL", "n.VC", "logL", "AIC", "BIC", "heritPEV", "heritVC", "A-opt", "D-opt")
    gfit <- data.frame(gfit)
    gfit[, 1] <- c("Independent", "id(Row):ar1(Col)", "ar1(Row):id(Col)", "ar1(Row):ar1(Col)", "BEST MODEL")
    if (sum(is.na(gfit[1:4, 2])) == 0) {
      gfit[5, 2] <- which.min(gfit[1:4, 2])
    }
    if (sum(is.na(gfit[1:4, 3])) == 0) {
      gfit[5, 3] <- which.max(gfit[1:4, 3])
    }
    if (sum(is.na(gfit[1:4, 4])) == 0) {
      gfit[5, 4] <- which.min(gfit[1:4, 4])
    }
    if (sum(is.na(gfit[1:4, 5])) == 0) {
      gfit[5, 5] <- which.min(gfit[1:4, 5])
    }
    if (sum(is.na(gfit[1:4, 6])) == 0) {
      gfit[5, 6] <- which.max(gfit[1:4, 6])
    }
    if (sum(is.na(gfit[1:4, 7])) == 0) {
      gfit[5, 7] <- which.max(gfit[1:4, 7])
    }
    if (sum(is.na(gfit[1:4, 8])) == 0) {
      gfit[5, 8] <- which.min(gfit[1:4, 8])
    }
    if (sum(is.na(gfit[1:4, 9])) == 0) {
      gfit[5, 9] <- which.min(gfit[1:4, 9])
    }

    # Reporting LRT to compare all models against independent (mod.ii)
    lrt.ic <- asreml::lrt.asreml(mod.ii, mod.ic, boundary = FALSE)[, 3]
    lrt.ri <- asreml::lrt.asreml(mod.ii, mod.ri, boundary = FALSE)[, 3]
    lrt.rc <- asreml::lrt.asreml(mod.ii, mod.rc, boundary = FALSE)[, 3]

    gfit$pvalue <- c(NA, lrt.ic, lrt.ri, lrt.rc, NA)
    return(gfit)
  }

  if (!is.null(model)) { # To report model and predictions
    if (model == 1) {
      eval(parse(text = str.mod.ii))
      if (!mod.ii$converge) {
        eval(parse(text = "mod.ii<-asreml::update.asreml(mod.ii)"))
      }
      mod <- mod.ii
    }
    if (model == 2) {
      eval(parse(text = str.mod.ic))
      if (!mod.ic$converge) {
        eval(parse(text = "mod.ic<-asreml::update.asreml(mod.ic)"))
      }
      mod <- mod.ic
    }
    if (model == 3) {
      eval(parse(text = str.mod.ri))
      if (!mod.ri$converge) {
        eval(parse(text = "mod.ri<-asreml::update.asreml(mod.ri)"))
      }
      mod <- mod.ri
    }
    if (model == 4) {
      eval(parse(text = str.mod.rc))
      if (!mod.rc$converge) {
        eval(parse(text = "mod.rc<-asreml::update.asreml(mod.rc)"))
      }
      mod <- mod.rc
    }
    if (type.gen == "fixed") {
      preds <- predict(mod, classify = "gen", vcov = TRUE)
      pvals <- preds$pvals
      vcov <- as.matrix(preds$vcov)
      sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
      sel[is.na(pvals$predicted.value), ] <- 0
      vcov <- vcov[sel == 1, sel == 1]
      pvals$weight[sel == 1] <- diag(solve(vcov))
    }
    if (type.gen == "random") {
      if (is.null(check)) { # No separation of checks
        preds <- predict(mod, classify = "gen", vcov = TRUE)
        pvals <- preds$pvals
        vcov <- as.matrix(preds$vcov)
        sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
        sel[is.na(pvals$predicted.value), ] <- 0
        vcov <- vcov[sel == 1, sel == 1]
        pvals$weight[sel == 1] <- diag(solve(vcov))
      }
      if (!is.null(check)) { # With separation of checks
        preds <- predict(mod, classify = "check:gen", vcov = TRUE)
        pvals <- preds$pvals
        vcov <- as.matrix(preds$vcov)
        sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
        sel[is.na(pvals$predicted.value), ] <- 0
        vcov <- vcov[sel == 1, sel == 1]
        pvals$weight[sel == 1] <- diag(solve(vcov))

        ctable <- data.frame(table(df$gen, df$check)) # Johan
        ctable$sel <- 0
        ctable$sel[ctable$Freq > 0] <- 1
        tt <- merge(pvals, ctable, by.x = c("check", "gen"), by.y = c("Var2", "Var1"))
        pvals <- tt[tt$sel == 1, ]
        pvals <- pvals[, -c(7, 8)]
      }
    }

    return(list(mod = mod, predictions = data.frame(pvals)))
  }
}


augment.SpATS <-
  function(x = NULL, col = "col", row = "row", response, all.in.one = TRUE, main = NULL, annotated = FALSE, depict.missing = FALSE, ...) {
    if (is.null(x)) {
      return()
    }

    Datos <- as.data.frame(x$mf)
    xlab <- col
    ylab <- row
    x.coord <- as.numeric(Datos[, col])
    y.coord <- as.numeric(Datos[, row])

    trait <- response
    response <- Datos[, response]
    residuals <- residuals(x)
    fitted <- fitted(x)

    columns <- seq(min(x.coord), max(x.coord), by = min(diff(sort(unique(x.coord)))))
    rows <- seq(min(y.coord), max(y.coord), by = min(diff(sort(unique(y.coord)))))
    xy.coord <- data.table::data.table(expand.grid(columns = columns, rows = rows))

    data.table::setNumericRounding(2)

    if (is.null(main)) main <- paste("Trait: ", trait, sep = "")

    if (length(grep("units", x$call)) != 0) {
      residuals_plot <- residuals(x, spatial = "plot")
      data.table::setkeyv(xy.coord, c("rows", "columns"))
      ONE <- rep(1, length(x.coord))
      df <- data.table::data.table(
        columns = x.coord, rows = y.coord,
        response = response,
        fitted = fitted,
        residuals = residuals,
        residuals_plot = residuals_plot, ONE = ONE
      )
      data.table::setkeyv(df, c("rows", "columns"))
      df <- df[xy.coord]
      df <- df[order(df$columns, df$rows), ]

      colors <- topo.colors(100)

      main.legends <- c("Raw data", "Fitted data", "Residuals-Trend", "Residuals-Plot")
      if (all.in.one) {
        op <- par(mfrow = c(1, 4), oma = c(ifelse(annotated, 12, 2), 1, 3, 2), mar = c(2.7, 4, 2.5, 2.5), mgp = c(1.7, 0.5, 0))
      } else {
        if (!is.null(main)) {
          main.legends <- rep(main, length(main.legends))
        }
      }

      range <- range(c(response, fitted), na.rm = TRUE)
      fields::image.plot(columns, rows, t(matrix(df$response, ncol = length(columns), nrow = length(rows))), main = main.legends[1], col = colors, xlab = xlab, ylab = ylab, zlim = range, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$fitted, ncol = length(columns), nrow = length(rows))), main = main.legends[2], col = colors, xlab = xlab, ylab = ylab, zlim = range, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$residuals, ncol = length(columns), nrow = length(rows))), main = main.legends[3], col = colors, xlab = xlab, ylab = ylab, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$residuals_plot, ncol = length(columns), nrow = length(rows))), main = main.legends[4], col = colors, xlab = xlab, ylab = ylab, graphics.reset = TRUE, ...)
      title("")
      mtext(main, cex = 1.5, outer = TRUE, side = 3)
    } else {
      data.table::setkeyv(xy.coord, c("rows", "columns"))
      ONE <- rep(1, length(x.coord))
      df <- data.table::data.table(
        columns = x.coord, rows = y.coord,
        response = response,
        fitted = fitted,
        residuals = residuals, ONE = ONE
      )
      data.table::setkeyv(df, c("rows", "columns"))
      df <- df[xy.coord]
      df <- df[order(df$columns, df$rows), ]

      colors <- topo.colors(100)

      main.legends <- c("Raw data", "Fitted data", "Residuals")
      if (all.in.one) {
        op <- par(mfrow = c(1, 3), oma = c(ifelse(annotated, 12, 2), 1, 3, 2), mar = c(2.7, 4, 2.5, 2.5), mgp = c(1.7, 0.5, 0))
      } else {
        if (!is.null(main)) {
          main.legends <- rep(main, length(main.legends))
        }
      }

      range <- range(c(response, fitted), na.rm = TRUE)
      fields::image.plot(columns, rows, t(matrix(df$response, ncol = length(columns), nrow = length(rows))), main = main.legends[1], col = colors, xlab = xlab, ylab = ylab, zlim = range, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$fitted, ncol = length(columns), nrow = length(rows))), main = main.legends[2], col = colors, xlab = xlab, ylab = ylab, zlim = range, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$residuals, ncol = length(columns), nrow = length(rows))), main = main.legends[3], col = colors, xlab = xlab, ylab = ylab, graphics.reset = TRUE, ...)
      title("")
      mtext(main, cex = 1.5, outer = TRUE, side = 3)
    }
  }




fill.asreml <- function(x, rows = NULL, ranges = NULL, by, extra) {
  if (is.null(rows) | rows == "") {
    return()
  }
  if (is.null(ranges) | ranges == "") {
    return()
  }

  checo <- which(colnames(x) %in% c(rows, ranges))
  if (length(checo) != 2) {
    stop("Please double check the rows and ranges argument that you provided. We did not find such columns.\n")
  }
  roro <- x[, which(colnames(x) == rows)]
  raro <- x[, which(colnames(x) == ranges)]
  if (!is.numeric(roro) | !is.numeric(raro)) {
    stop("Please make sure that the columns; ", rows, " and ",
      ranges, " are both numeric.\n",
      call. = FALSE
    )
  }
  if (!missing(extra)) {
    extras <- TRUE
  } else {
    extras <- FALSE
  }
  fac <- which(unlist(lapply(x, class)) == "factor")
  if (length(fac) > 0) {
    for (u in 1:length(fac)) {
      fac2 <- fac[u]
      x[, fac2] <- as.character(x[, fac2])
    }
  }
  if (!missing(by)) {
    y <- split(x, x[, by])
    xnew <- lapply(y, function(x, extra, extras) {
      x <- x[order(x[, rows], x[, ranges]), ]
      roro <- x[, which(colnames(x) == rows)]
      raro <- x[, which(colnames(x) == ranges)]
      bybo <- na.omit(unique(x[, by]))
      ro1 <- seq(min(roro, na.rm = TRUE), max(roro, na.rm = TRUE))
      ra1 <- seq(min(raro, na.rm = TRUE), max(raro, na.rm = TRUE))
      needed <- expand.grid(ro1, ra1)
      colnames(needed) <- c(rows, ranges)
      needed <- needed[order(needed[, rows], needed[
        ,
        ranges
      ]), ]
      head(needed)
      x <- x[order(x[, rows], x[, ranges]), ]
      head(x)
      dis <- dim(x)
      dis2 <- dim(needed)
      newf <- data.frame(matrix(NA, dis2[1], dis[2]))
      colnames(newf) <- colnames(x)
      head(newf)
      sto <- rownames(x)
      rownames(x) <- paste(x[, rows], x[, ranges], sep = ".")
      rownames(needed) <- rownames(newf) <- paste(needed[
        ,
        rows
      ], needed[, ranges], sep = ".")
      newf[rownames(x), ] <- x
      newf[, c(rows, ranges)] <- needed
      head(newf)
      rownames(newf) <- NULL
      newf[, by] <- bybo
      if (extras) {
        for (u in 1:length(extra)) {
          pextra <- extra[u]
          pox <- table(newf[, c(rows, ranges, pextra)])
          leve <- dim(pox)[3]
          levelnames <- na.omit(unique(newf[, pextra]))
          for (o in 1:leve) {
            init1 <- which(apply(
              as.matrix(pox[, , o]),
              1, sum
            ) > 0)
            stend1 <- c(init1[1], init1[length(init1)])
            init2 <- which(apply(
              as.matrix(pox[, , o]),
              2, sum
            ) > 0)
            stend2 <- c(init2[1], init2[length(init2)])
            kk <- which(newf[, rows] >= stend1[1] &
              newf[, rows] <= stend1[2] & newf[, ranges] >=
              stend2[1] & newf[, ranges] <= stend2[2])
            newf[kk, pextra] <- levelnames[o]
          }
        }
      }
      return(newf)
    }, extra = extra, extras = extras)
    xnew <- do.call(rbind, xnew)
  } else {
    roro <- x[, which(colnames(x) == rows)]
    raro <- x[, which(colnames(x) == ranges)]
    cat("Argument 'by' not provided. Single field assumed.\n")
    ro1 <- seq(min(roro, na.rm = TRUE), max(roro, na.rm = TRUE))
    ra1 <- seq(min(raro, na.rm = TRUE), max(raro, na.rm = TRUE))
    needed <- expand.grid(ro1, ra1)
    colnames(needed) <- c(rows, ranges)
    needed <- needed[order(needed[, rows], needed[, ranges]), ]
    head(needed)
    x <- x[order(x[, rows], x[, ranges]), ]
    head(x)
    dis <- dim(x)
    dis2 <- dim(needed)
    newf <- data.frame(matrix(NA, dis2[1], dis[2]))
    colnames(newf) <- colnames(x)
    head(newf)
    sto <- rownames(x)
    rownames(x) <- paste(x[, rows], x[, ranges], sep = ".")
    rownames(needed) <- rownames(newf) <- paste(needed[
      ,
      rows
    ], needed[, ranges], sep = ".")
    newf[rownames(x), ] <- x
    newf[, c(rows, ranges)] <- needed
    head(newf)
    rownames(newf) <- NULL
    if (extras) {
      for (u in 1:length(extra)) {
        pextra <- extra[u]
        pox <- table(newf[, c(rows, ranges, pextra)])
        leve <- dim(pox)[3]
        levelnames <- na.omit(unique(newf[, pextra]))
        for (o in 1:leve) {
          init1 <- which(apply(
            as.matrix(pox[, , o]),
            1, sum
          ) > 0)
          stend1 <- c(init1[1], init1[length(init1)])
          init2 <- which(apply(
            as.matrix(pox[, , o]),
            2, sum
          ) > 0)
          stend2 <- c(init2[1], init2[length(init2)])
          kk <- which(newf[, rows] >= stend1[1] & newf[
            ,
            rows
          ] <= stend1[2] & newf[, ranges] >= stend2[1] &
            newf[, ranges] <= stend2[2])
          newf[kk, pextra] <- levelnames[o]
        }
      }
    }
    xnew <- newf
  }
  return(xnew)
}



# Prediction --------------------------------------------------------------


pred_ran_aug <- function(model, gen = "gen") {
  intercept <- data.frame(summary(model, coef = T)$coef.fixed)["(Intercept)", 1]
  gen_names <- levels(as.factor(model$mf[, gen]))
  gen_effects <- data.frame(summary(model, coef = T)$coef.random) %>%
    tibble::rownames_to_column("gen") %>%
    dplyr::select(-z.ratio)
  gen_effects <- gen_effects[grepl(gen, gen_effects$gen), ]
  gen_effects <- gen_effects %>%
    dplyr::mutate(
      solution = intercept + solution,
      lower = solution - 1.645 * std.error,
      upper = solution + 1.645 * std.error
    )
  gen_effects$gen <- gen_names
  return(gen_effects)
}

pred_fix_aug <- function(model, gen = "gen") {
  fix_coef <- data.frame(summary(model, coef = T)$coef.fixed)["(Intercept)", ]
  gen_names <- levels(as.factor(model$mf[, gen]))
  gen_effects <- data.frame(summary(model, coef = T)$coef.fixed) %>%
    tibble::rownames_to_column("gen") %>%
    dplyr::select(-z.ratio) %>%
    .[1:length(gen_names), ] %>%
    dplyr::mutate(gen = gen_names)

  gen_effects <- gen_effects %>% dplyr::mutate(solution = fix_coef[, 1] + solution)
  gen_effects[1, 2] <- fix_coef[, 1]
  gen_effects[1, 3] <- fix_coef[, 2]
  gen_effects <- gen_effects %>% dplyr::mutate(
    lower = solution - 1.645 * std.error,
    upper = solution + 1.645 * std.error
  )
  return(gen_effects)
}

pred_check_aug <- function(model, gen = "gen") {
  fix_coef <- data.frame(summary(model, coef = T)$coef.fixed)["(Intercept)", ]
  gen_names <- levels(as.factor(model$mf[, gen]))

  check_fixed <- data.frame(summary(model, coef = T)$coef.fixed) %>% tibble::rownames_to_column("gen")
  check_fixed <- check_fixed[grepl("control", check_fixed$gen), ]
  check_fixed <- check_fixed %>% dplyr::filter(solution != 0 & !is.na(std.error))
  check_fixed <- check_fixed %>%
    dplyr::mutate(gen = gen_names[1:dplyr::n()]) %>%
    dplyr::select(-z.ratio)
  check_fixed$check <- TRUE
  test_random <- data.frame(summary(model, coef = T)$coef.ran) %>% tibble::rownames_to_column("gen")
  test_random <- test_random[grepl("test", test_random$gen), ]
  test_random <- test_random %>% dplyr::filter(solution != 0 & !is.na(std.error))
  test_random <- test_random %>%
    dplyr::mutate(gen = gen_names[1:dplyr::n()]) %>%
    dplyr::select(-z.ratio)
  test_random$check <- FALSE
  test_random <- test_random[!test_random$gen %in% check_fixed$gen, ]

  gen_effects <- rbind(check_fixed, test_random) %>%
    dplyr::mutate(
      solution = fix_coef[, 1] + solution,
      lower = solution - 1.645 * std.error,
      upper = solution + 1.645 * std.error
    )
  return(gen_effects)
}

prediction_augment <- function(model, gen = "gen") {
  fixed_components <- data.frame(summary(model, coef = T)$coef.fixed) %>% tibble::rownames_to_column("coef")
  opt_1 <- sum(grepl("at", fixed_components$coef)) >= 1
  if (opt_1) {
    effects <- pred_check_aug(model, gen = gen)
  } else {
    opt_2 <- sum(grepl(gen, fixed_components$coef)) >= 1
    if (opt_2) {
      effects <- pred_fix_aug(model, gen = gen)
    } else {
      effects <- pred_ran_aug(model, gen = "gen")
    }
  }
  return(effects)
}




# New Spatial Plot Asreml -------------------------------------------------

spatial.ASReml <-
  function(x = NULL, col = "col", row = "row", genotype, response, all.in.one = TRUE, main = NULL, annotated = FALSE, depict.missing = FALSE, ...) {
    if (is.null(x)) {
      return()
    }

    Datos <- as.data.frame(x$mf)
    xlab <- col
    ylab <- row
    x.coord <- as.numeric(Datos[, col])
    y.coord <- as.numeric(Datos[, row])

    trait <- response
    response <- Datos[, response]
    residuals <- residuals(x)
    fitted <- fitted(x)

    columns <- seq(min(x.coord), max(x.coord), by = min(diff(sort(unique(x.coord)))))
    rows <- seq(min(y.coord), max(y.coord), by = min(diff(sort(unique(y.coord)))))
    xy.coord <- data.table::data.table(expand.grid(columns = columns, rows = rows))

    data.table::setNumericRounding(2)

    if (is.null(main)) main <- paste("Trait: ", trait, sep = "")

    if (length(grep("units", x$call)) != 0) {
      residuals_plot <- residuals(x, spatial = "plot")
      data.table::setkeyv(xy.coord, c("rows", "columns"))
      ONE <- rep(1, length(x.coord))
      df <- data.table::data.table(
        columns = x.coord, rows = y.coord,
        response = response,
        fitted = fitted,
        residuals = residuals,
        residuals_plot = residuals_plot, ONE = ONE
      )
      data.table::setkeyv(df, c("rows", "columns"))
      df <- df[xy.coord]
      df <- df[order(df$columns, df$rows), ]

      # genotype
      Datos[, col] <- as.numeric(Datos[, col])
      Datos[, row] <- as.numeric(Datos[, row])
      df <- merge(df, Datos[, c(col, row, genotype)], by.x = c("columns", "rows"), by.y = c(col, row), sort = F, all.x = T)
      names(df)[ncol(df)] <- "geno.pred"

      check_gen <- sum(grep(genotype, x$formulae$fixed))

      if (check_gen == 0) {
        geno.pred <- coef(x)$random %>% data.frame(gen = rownames(.), BLUP = .)
      } else {
        geno.pred <- coef(x)$fixed %>% data.frame(gen = rownames(.), BLUE = .)
      }
      geno.pred <- geno.pred[grep(pattern = genotype, geno.pred$gen), ]
      geno.pred$gen <- gsub(pattern = paste0(genotype, "_"), replacement = "", x = rownames(geno.pred))
      names(geno.pred)[1] <- "geno.pred"
      df <- merge(df, geno.pred, by = c("geno.pred"), sort = F, all.x = T)
      names(df)[ncol(df)] <- "effect"

      # environment
      df <- df %>% dplyr::mutate(environment = round(fitted - mean(fitted, na.rm = T) - effect, 2))

      colors <- topo.colors(100)

      main.legends <- c("Raw data", "Fitted data", "Residuals-Trend", "Env", "Genotype", "Histogram")
      if (all.in.one) {
        op <- par(mfrow = c(2, 3), oma = c(ifelse(annotated, 12, 2), 1, 3, 2), mar = c(2.7, 4, 2.5, 2.5), mgp = c(1.7, 0.5, 0))
      } else {
        if (!is.null(main)) {
          main.legends <- rep(main, length(main.legends))
        }
      }

      range <- range(c(response, fitted), na.rm = TRUE)
      range <- range(c(response), na.rm = TRUE)
      fields::image.plot(columns, rows, t(matrix(df$response, ncol = length(columns), nrow = length(rows))), main = main.legends[1], col = colors, xlab = xlab, ylab = ylab, zlim = range, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$fitted, ncol = length(columns), nrow = length(rows))), main = main.legends[2], col = colors, xlab = xlab, ylab = ylab, zlim = range, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$residuals, ncol = length(columns), nrow = length(rows))), main = main.legends[3], col = colors, xlab = xlab, ylab = ylab, graphics.reset = TRUE, ...)
      # fields::image.plot(columns, rows, t(matrix(df$residuals_plot, ncol = length(columns), nrow = length(rows))), main = main.legends[4], col = colors, xlab = xlab, ylab = ylab, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$environment, ncol = length(columns), nrow = length(rows))), main = main.legends[4], col = colors, xlab = xlab, ylab = ylab, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$effect, ncol = length(columns), nrow = length(rows))), main = main.legends[5], col = colors, xlab = xlab, ylab = ylab, graphics.reset = TRUE)
      suppressWarnings(hist(geno.pred$effect, main = main.legends[6], xlab = main.legends[5], ...))
      title("")
      mtext(main, cex = 1.5, outer = TRUE, side = 3)
    } else {
      data.table::setkeyv(xy.coord, c("rows", "columns"))
      ONE <- rep(1, length(x.coord))
      df <- data.table::data.table(
        columns = x.coord, rows = y.coord,
        response = response,
        fitted = fitted,
        residuals = residuals, ONE = ONE
      )
      data.table::setkeyv(df, c("rows", "columns"))
      df <- df[xy.coord]
      df <- df[order(df$columns, df$rows), ]

      # genotype
      Datos[, col] <- as.numeric(Datos[, col])
      Datos[, row] <- as.numeric(Datos[, row])
      df <- merge(df, Datos[, c(col, row, genotype)], by.x = c("columns", "rows"), by.y = c(col, row), sort = F, all.x = T)
      names(df)[ncol(df)] <- "geno.pred"

      check_gen <- sum(grep(genotype, x$formulae$fixed))

      if (check_gen == 0) {
        geno.pred <- coef(x)$random %>% data.frame(gen = rownames(.), BLUP = .)
      } else {
        geno.pred <- coef(x)$fixed %>% data.frame(gen = rownames(.), BLUE = .)
      }
      geno.pred <- geno.pred[grep(pattern = genotype, geno.pred$gen), ]
      geno.pred$gen <- gsub(pattern = paste0(genotype, "_"), replacement = "", x = rownames(geno.pred))
      names(geno.pred)[1] <- "geno.pred"
      df <- merge(df, geno.pred, by = c("geno.pred"), sort = F, all.x = T)
      names(df)[ncol(df)] <- "effect"

      # environment
      df <- df %>% dplyr::mutate(environment = round(fitted - mean(fitted, na.rm = T) - effect, 2))

      colors <- topo.colors(100)

      main.legends <- c("Raw data", "Fitted data", "Residuals", "Env", "Genotype", "Histogram")
      if (all.in.one) {
        op <- par(mfrow = c(2, 3), oma = c(ifelse(annotated, 12, 2), 1, 3, 2), mar = c(2.7, 4, 2.5, 2.5), mgp = c(1.7, 0.5, 0))
      } else {
        if (!is.null(main)) {
          main.legends <- rep(main, length(main.legends))
        }
      }

      range <- range(c(response, fitted), na.rm = TRUE)
      range <- range(c(response), na.rm = TRUE)
      fields::image.plot(columns, rows, t(matrix(df$response, ncol = length(columns), nrow = length(rows))), main = main.legends[1], col = colors, xlab = xlab, ylab = ylab, zlim = range, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$fitted, ncol = length(columns), nrow = length(rows))), main = main.legends[2], col = colors, xlab = xlab, ylab = ylab, zlim = range, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$residuals, ncol = length(columns), nrow = length(rows))), main = main.legends[3], col = colors, xlab = xlab, ylab = ylab, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$environment, ncol = length(columns), nrow = length(rows))), main = main.legends[4], col = colors, xlab = xlab, ylab = ylab, graphics.reset = TRUE, ...)
      fields::image.plot(columns, rows, t(matrix(df$effect, ncol = length(columns), nrow = length(rows))), main = main.legends[5], col = colors, xlab = xlab, ylab = ylab, graphics.reset = TRUE)
      suppressWarnings(hist(geno.pred$effect, main = main.legends[6], xlab = main.legends[5], ...))
      title("")
      mtext(main, cex = 1.5, outer = TRUE, side = 3)
    }
  }
