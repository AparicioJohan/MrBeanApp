#' Routine to fit a given spatial models for field trials
#'
#' \code{spatial.single} Fit the spatial model according to the specification of conditions
#' given a provided dataset. It also calculates goodness-of-fit statisitcs to later compare
#' with other spatial model. This routine works best for replicated genotypes.
#' Some checks on data are incorporated, but data is assumed to come in a full grid.
#'
#' @param data dataframe with all relevant columns for spatial model and response variables.
#' @param gen factor name for genotypes (or treatments)
#' @param block factor name for full block (or replicates) (optional)
#' @param ibk factor name for incomplete block (optional) (optional)
#' @param row column name for row coordinates of each experimental unit
#' @param col column name for column coordinates of each experimental unit
#' @param cov1 column name with additional covariate 1 (optional)
#' @param cov2 column name with additional covariate 2 (optional)
#' @param resp column name for the response variable to analyze
#' @param add.block logical to add to model block effects (default = FALSE)
#' @param add.ibk logical to fit incomplete block random effects (default = FALSE)
#' @param add.row logical to fit row within block random effects (default = FALSE)
#' @param add.col logical to fit column within block random effects (default = FALSE)
#' @param add.spl.row logical to fit splines across rows (default = FALSE)
#' @param add.spl.col logical to fit splines across columns (default = FALSE)
#' @param add.cov1 logical to fit additional covariate 1 (default = FALSE)
#' @param add.cov2 logical to fit additional covariate 2 (default = FALSE)
#' @param add.nugget logical to fit nugget random effects (only for autoregressive errors) (default = FALSE)
#' @param type.gen model assumption for genotypes: 'random' or 'fixed' (default = 'random')
#' @param type.block model assumption for full blocks: 'random' or 'fixed' (default = 'fixed')
#' @param type.residual model assumption for residual terms: 'indep' or 'ar1' (default = 'ar1')
#'
#' @return Several objects with details of the fitted model.
#' aov:         Wald-test (mixed model ANOVA-like table)
#' call:        String with the ASReml-R call used to fit the requested model
#' gt:          Goodness-of-fit statistics for the model evaluated are reported as summary. This includes columns:
#'               number of variance components in the model (n.VC), log-likelihood (logL), Akaike information
#'               criteria (AIC), Bayesian information criteria (BIC), A-optimality value, logarithm of the
#'               D-optimality value, and heritability based on predictor error variance (heritPEV, only for
#'               genotypes 'random')
#' mod:         ASReml-R object with all information from the fitted model
#' predictions: Predictions for all genotypes, with an additional column of 'weight' is to be used on a
#'               on a second-stage analysis. These weights are the diagonal of the inverse of the variance-
#'               covariance matrix of predictions
#'
#' @author
#' Salvador A. Gezan. VSN International
#'
#' @examples
#' # Example 1: Replicated Trial - Genotype random + spatial terms
#' # library(agridat)
#' # testREP <- durban.rowcol
#' # testREP$bed <- as.factor(testREP$bed)
#' # testREP$row <- as.factor(testREP$row)
#' # testREP$gen <- as.factor(testREP$gen)
#' # head(testREP)
#' # output.REP <- spatial.single(data=testREP, gen='gen', row='row', col='bed', resp='yield',
#' #                             add.block=TRUE,  add.row=TRUE,  add.col=TRUE,
#' #                             type.gen='random', type.block='fixed', type.residual='ar1')
#' # output.REP$gt$herit.PEV           # Heritability-PEV
#' # output.REP$aov                    # Wald Test
#' # summary(output.REP$mod)$varcomp   # Variance Components
#' # head(output.REP$predictions)
#' #
#' # # Example 2: Simpler model - independent errors
#' # output.ID <- spatial.single(data=testREP, gen='gen', row='row', col='bed', resp='yield',
#' #                             add.block=TRUE,  add.row=TRUE,  add.col=TRUE,
#' #                             type.gen='random', type.block='fixed', type.residual='indep')
#' # output.ID$gt$herit.PEV           # Heritability-PEV
#' # output.ID$aov                    # Wald Test
#' # summary(output.ID$mod)$varcomp   # Variance Components
#' # head(output.ID$predictions)
#' #
#' # # Example 3: Comparing Models with a LRT
#' # lrt.asreml(output.REP$mod, output.ID$mod, boundary=FALSE)
#'
spatial.single <- function(data = NULL, gen = NULL, block = NULL, ibk = NULL, row = NULL,
                           col = NULL, cov1 = NULL, cov2 = NULL, resp = NULL,
                           add.block = FALSE, add.ibk = FALSE, add.row = FALSE, add.col = FALSE,
                           add.spl.row = FALSE, add.spl.col = FALSE, add.cov1 = FALSE, add.cov2 = FALSE,
                           add.nugget = FALSE, type.gen = "random", type.block = "fixed", type.residual = "ar1") {
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
  if (!is.null(block)) {
    df$block <- as.factor(data[, block])
    if (type.block != "fixed" & type.block != "random") {
      stop("Specification of blocks (random/fixed) not indicated or incorrect.")
    }
  }
  if (!is.null(ibk)) {
    df$ibk <- as.factor(data[, ibk])
  }
  if (!is.null(cov1)) {
    df$cov1 <- as.numeric(data[, cov1])
  }
  if (!is.null(cov2)) {
    df$cov2 <- as.numeric(data[, cov2])
  }
  if (is.null(resp)) {
    stop("No response variable indicated.")
  } else {
    df$resp <- data[, resp]
  }
  if (type.gen != "fixed" & type.gen != "random") {
    stop("Specification of genotypes (random/fixed) not indicated or incorrect.")
  }
  if (type.residual != "indep" & type.residual != "ar1") {
    stop("Specification of type of residuals (indep/ar1) not indicated or incorrect.")
  }

  # Code Strings for ASReml-R
  code.asr <- as.character()
  code.asr[1] <- "asreml::asreml(fixed=resp~1"
  code.asr[2] <- "random=~"
  code.asr[3] <- "residual=~"
  code.asr[4] <- 'na.action=list(x=\"include\",y=\"include\"),data=df)'
  nrand <- 0 # Number of random terms
  # mod.sp<-paste(code.asr[1],code.asr[2],code.asr[3],code.asr[4],sep=',')
  # mod.sp

  # Adding cov1 (fixed)
  if (!is.null(cov1) & add.cov1) {
    code.asr[1] <- paste(code.asr[1], "cov1", sep = "+")
  }
  # Adding cov2 (fixed)
  if (!is.null(cov2) & add.cov2) {
    code.asr[1] <- paste(code.asr[1], "cov2", sep = "+")
  }
  # Adding blocks (random or fixed)
  if (!is.null(block) & add.block) { #
    if (type.block == "random") {
      code.asr[2] <- paste(code.asr[2], "block", sep = "+")
      nrand <- nrand + 1
    }
    if (type.block == "fixed") {
      code.asr[1] <- paste(code.asr[1], "block", sep = "+")
    }
  }
  # Adding block:ibk (random)
  if (!is.null(ibk) & add.ibk) {
    if (!add.block) { # No blocks
      code.asr[2] <- paste(code.asr[2], "ibk", sep = "+")
      nrand <- nrand + 1
    }
    if (!is.null(block) & add.block) { # with blocks
      code.asr[2] <- paste(code.asr[2], "block:ibk", sep = "+")
      nrand <- nrand + 1
    }
  }
  # Adding block:row (random)
  if (!is.null(row) & add.row) {
    if (!add.block) { # No blocks
      code.asr[2] <- paste(code.asr[2], "row", sep = "+")
      nrand <- nrand + 1
    }
    if (!is.null(block) & add.block) { # with blocks
      code.asr[2] <- paste(code.asr[2], "block:row", sep = "+")
      nrand <- nrand + 1
    }
  }
  # Adding block:col (random)
  if (!is.null(col) & add.col) {
    if (!add.block) { # No blocks
      code.asr[2] <- paste(code.asr[2], "col", sep = "+")
      nrand <- nrand + 1
    }
    if (!is.null(block) & add.block) { # with blocks
      code.asr[2] <- paste(code.asr[2], "block:col", sep = "+")
      nrand <- nrand + 1
    }
  }
  # Adding spl.row (random and fixed lin)
  if (add.spl.row) {
    code.asr[1] <- paste(code.asr[1], "lin(row)", sep = "+")
    code.asr[2] <- paste(code.asr[2], "spl(row)", sep = "+")
    nrand <- nrand + 1
  }
  # Adding spl.col (random and fixed lin)
  if (add.spl.col) {
    code.asr[1] <- paste(code.asr[1], "lin(col)", sep = "+")
    code.asr[2] <- paste(code.asr[2], "spl(col)", sep = "+")
    nrand <- nrand + 1
  }
  # Adding gen (fixed or random)
  if (type.gen == "fixed") {
    code.asr[1] <- paste(code.asr[1], "gen", sep = "+")
  }
  if (type.gen == "random") {
    code.asr[2] <- paste(code.asr[2], "gen", sep = "+")
    nrand <- nrand + 1
  }
  # Specifying residual type and adding nugget (only ar1)
  if (type.residual == "indep") {
    code.asr[3] <- "residual=~id(row):idv(col)"
  }
  if (type.residual == "ar1") {
    if (!add.nugget) {
      code.asr[3] <- "residual=~ar1(row):ar1v(col)"
    }
    if (add.nugget) {
      code.asr[3] <- "residual=~ar1(row):ar1v(col)"
      code.asr[2] <- paste(code.asr[2], "units", sep = "+")
      nrand <- nrand + 1
    }
  }

  # Running final spatial models in ASReml-R
  df <- df[order(df$row, df$col), ] # order data by row then column
  code.asr[1] <- paste("mod.ref<-", code.asr[1], sep = "")
  if (nrand == 0) {
    str.mod <- paste(code.asr[1], code.asr[3], code.asr[4], sep = ",")
  }
  if (nrand != 0) {
    str.mod <- paste(code.asr[1], code.asr[2], code.asr[3], code.asr[4], sep = ",")
  }
  eval(parse(text = str.mod))
  if (!mod.ref$converge) {
    eval(parse(text = "mod.ref<-asreml::update.asreml(mod.ref)"))
  }
  # print(str.mod)

  # Obtaining predictions for models (and ANOVA)
  aov <- asreml::wald.asreml(mod.ref, denDF = "algebraic", ssType = "incremental")$Wald
  preds <- predict(mod.ref, classify = "gen", vcov = TRUE)
  # Obtaining solutions (only random)
  if (type.gen == "fixed") {
    sols <- NULL
  }
  if (type.gen == "random") {
    sols <- summary(mod.ref, coef = TRUE)$coef.random
  }
  # Calling the statistics
  gt <- stats.spatial(object = mod.ref, checks = FALSE, solution = sols, preds = preds)
  # Adding Weights
  pvals <- preds$pvals
  vcov <- as.matrix(preds$vcov)
  sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
  sel[is.na(pvals$predicted.value), ] <- 0
  vcov <- vcov[sel == 1, sel == 1]
  pvals$weight[sel == 1] <- diag(solve(vcov))

  return(list(call = str.mod, mod = mod.ref, gt = gt, predictions = pvals, aov = aov))
}



#' Routine to fit several spatial models and suggest the best model
#'
#' \code{spatial.selector} Generates a table with the goodness-of-fit statitics to select the
#' best spatial model for an analysis of different analyses (unreplicated and replicated).
#' Models that can not fit are reported as NA.
#' The models require some input conditions from user: 1) Genotypes: fixed/random; 2)	Blocks:
#' TRUE/FALSE (if TRUE then fixed/random); 3) Covariates (always fixed, max 2); and 4)
#' Incomplete Blocks: TRUE/FALSE (always random).
#' The model selector will run over a series of models with the conditions: 1) Rows within
#' replicate: TRUE/FALSE (always random); 2) Columns within replicate: TRUE/FALSE (always
#' random); 3) Spline on rows: TRUE/FALSE (fixed linear covariate + random spline); 4) Spline
#' on columns: TRUE/FALSE (fixed linear covariate + random spline); and 5) Residual structure:
#' 'indep' or 'ar1'. In addition the best spatial model (with ar1) and selected according to
#' A-optimality is fitted with an additional nugget effect. List of 32 models is found on data
#' MODLIST.Rda.
#'
#' @param data dataframe with all relevant columns for spatial model and response variables.
#' @param gen factor name for genotypes (or treatments)
#' @param block factor name for full block (or replicates) (optional)
#' @param ibk factor name for incomplete block (optional) (optional)
#' @param row column name for row coordinates of each experimental unit
#' @param col column name for column coordinates of each experimental unit
#' @param cov1 column name with additional covariate 1 (optional)
#' @param cov2 column name with additional covariate 2 (optional)
#' @param resp column name for the response variable to analyze
#' @param type.gen model assumption for genotypes: 'random' or 'fixed' (default: 'random')
#' @param type.block model assumption for full blocks: 'random' or 'fixed' (default: 'fixed')
#' @param nugget logical to add nugget to any spatial model fitted (dafualt: FALSE)
#' @param model model number to be fitted (optional)
#'
#' @return A table with goodness-of-fit statistics for models evaluated. This includes columns:
#'    number of variance components in the model (n.VC), log-likelihood (logL), Akaike information
#'    criteria (AIC), Bayesian information criteria (BIC), and heritability based on predictor
#'    error variance (heritPEV, only for genotypes 'random').
#'
#'    If parameter 'model' is specified the object with fitted model number is provided, with
#'    several objects with details of the fitted model.
#'
#'    aov:         Wald-test (mixed model ANOVA-like table)
#'    call:        String with the ASReml-R call used to fit the requested model
#'    gt:          Goodness-of-fit statistics for the model evaluated are reported as summary. This includes columns:
#'                  number of variance components in the model (n.VC), log-likelihood (logL), Akaike information
#'                  criteria (AIC), Bayesian information criteria (BIC), A-optimality value, logarithm of the
#'                  D-optimality value, and heritability based on predictor error variance (heritPEV, only for
#'                  genotypes 'random')
#'    mod:         ASReml-R object with all information from the fitted model
#'    predictions: Predictions for all genotypes, with an additional column of 'weight' is to be used on a
#'                  on a second-stage analysis. These weights are the diagonal of the inverse of the variance-
#'                  covariance matrix of predictions
#'
#' @author
#' Salvador A. Gezan. VSN International
#'
#' @examples
#' # Example 1: Selecting best model from Replicated Trial
#' # library(agridat)
#' # testREP <- durban.rowcol
#' # testREP$bed <- as.factor(testREP$bed)
#' # testREP$row <- as.factor(testREP$row)
#' # testREP$gen <- as.factor(testREP$gen)
#' # head(testREP)
#' # test.sel <- spatial.selector(data=testREP, gen='gen', row='row', col='bed',
#' #                              resp='yield', type.gen='random')
#' # View(test.sel$parms)
#' #
#' # # Example 2: Fitting only selected model 27
#' # mod.sel <- spatial.selector(data=testREP, gen='gen', row='row', col='bed',
#' #                              resp='yield', type.gen='random', model=27)
#' # ls(mod.sel)
#' # mod.sel$call                  # Call for model fitted
#' # plot(mod.sel$mod)             # Residual plots
#' # plot(varioGram(mod.sel$mod))  # Variogram fitted model
#' # summary(mod.sel$mod)$varcomp  # Variance Components
#' # mod.sel$aov                   # Wald-test Table
#' # mod.sel$gt$herit.PEV          # Heritability (other statistics on gt)
#' # head(mod.sel$predictions)     # Predictions for fitted model with additional weights
#'
spatial.selector <- function(data = NULL, gen = NULL, block = NULL, ibk = NULL, row = NULL, col = NULL,
                             cov1 = NULL, cov2 = NULL, resp = NULL,
                             type.gen = "random", type.block = "fixed", nugget = FALSE, model = NULL) {

  # load(file='MODLIST.Rda')
  # load(system.file("../R/MODLIST.Rda", package = "MrBean")) was added in data folder
  # Generic parameters for all models
  if (!is.null(block)) {
    add.block <- TRUE
    type.block <- type.block
  }
  if (is.null(block)) {
    add.block <- FALSE
    type.block <- NULL
  } ### Fixed
  if (!is.null(ibk)) {
    add.ibk <- TRUE
  }
  if (is.null(ibk)) {
    add.ibk <- FALSE
  }
  if (!is.null(cov1)) {
    add.cov1 <- TRUE
  }
  if (is.null(cov1)) {
    add.cov1 <- FALSE
  }
  if (!is.null(cov2)) {
    add.cov2 <- TRUE
  }
  if (is.null(cov2)) {
    add.cov2 <- FALSE
  }

  if (is.null(model)) {

    # Loop over all models in mod.list
    gfit <- matrix(NA, ncol = 9, nrow = 33)
    for (i in 1:32) {
      # print(i)
      add.row <- mod.list$RepRow[i]
      add.col <- mod.list$RepCol[i]
      add.spl.row <- mod.list$splineRow[i]
      add.spl.col <- mod.list$splineCol[i]
      type.residual <- mod.list$Resid[i]
      if (type.residual == "ar1") {
        if (nugget) {
          add.nugget <- TRUE
        }
        if (!nugget) {
          add.nugget <- mod.list$Nugget[i]
        }
      }
      mod.ref <- spatial.single(
        data = data, gen = gen, block = block, ibk = ibk, row = row,
        col = col, cov1 = cov1, cov2 = cov2, resp = resp,
        add.block = add.block, add.ibk = add.ibk,
        add.row = add.row, add.col = add.col,
        add.spl.row = add.spl.row, add.spl.col = add.spl.col,
        add.cov1 = add.cov1, add.cov2 = add.cov2,
        add.nugget = add.nugget, type.gen = type.gen,
        type.block = type.block, type.residual = type.residual
      )
      # Storing the statistics
      gt <- mod.ref$gt
      gfit[i, 1] <- mod.list$Model[i]
      gfit[i, 2] <- gt$n.vc
      gfit[i, 3] <- gt$logL
      gfit[i, 4] <- gt$aic
      gfit[i, 5] <- gt$bic
      gfit[i, 6] <- gt$herit.PEV
      gfit[i, 7] <- gt$herit.VC
      gfit[i, 8] <- gt$Aopt
      gfit[i, 9] <- gt$Dopt
    }

    colnames(gfit) <- c("MODEL", "n.VC", "logL", "AIC", "BIC", "heritPEV", "heritVC", "A-opt", "D-opt")
    gfit <- data.frame(gfit)
    gfit <- data.frame(gfit, mod.list[, -1])

    # Identifying best model according to A-opt - and fitting with nugget
    best.m <- which.min(gfit$A.opt)
    if (length(best.m) == 0) {
      best.m <- 0
    }
    if (best.m > 17 & best.m < 32) {
      add.row <- mod.list$RepRow[best.m]
      add.col <- mod.list$RepCol[best.m]
      add.spl.row <- mod.list$splineRow[best.m]
      add.spl.col <- mod.list$splineCol[best.m]
      type.residual <- mod.list$Resid[best.m]
      mod.ref <- spatial.single(
        data = data, gen = gen, block = block, ibk = ibk, row = row,
        col = col, cov1 = cov1, cov2 = cov2, resp = resp,
        add.block = add.block, add.ibk = add.ibk,
        add.row = add.row, add.col = add.col,
        add.spl.row = add.spl.row, add.spl.col = add.spl.col,
        add.cov1 = add.cov1, add.cov2 = add.cov2,
        add.nugget = TRUE, type.gen = type.gen,
        type.block = type.block, type.residual = type.residual
      )
      # Storing the statistics for model 33
      gt <- mod.ref$gt
      gfit[33, 1] <- mod.list$Model[33]
      gfit[33, 2] <- gt$n.vc
      gfit[33, 3] <- gt$logL
      gfit[33, 4] <- gt$aic
      gfit[33, 5] <- gt$bic
      gfit[33, 6] <- gt$herit.PEV
      gfit[33, 7] <- gt$herit.VC
      gfit[33, 8] <- gt$Aopt
      gfit[33, 9] <- gt$Dopt
      gfit$RepRow[33] <- mod.list$RepRow[best.m]
      gfit$RepCol[33] <- mod.list$RepCol[best.m]
      gfit$splineRow[33] <- mod.list$splineRow[best.m]
      gfit$splineCol[33] <- mod.list$splineCol[best.m]
      gfit$Resid[33] <- mod.list$Resid[best.m]
      # gfit <- data.frame(gfit,mod.list[,-1])
    } else {
      warning("Model with nugget cannot be fitted as best model is not spatial, or it can not be identified.")
      gfit[33, 1] <- mod.list$Model[33]
      gfit[33, 2] <- NA
      gfit[33, 3] <- NA
      gfit[33, 4] <- NA
      gfit[33, 5] <- NA
      gfit[33, 6] <- NA
      gfit[33, 7] <- NA
      gfit[33, 8] <- NA
      gfit[33, 9] <- NA
    }

    return(list(mod = NULL, parms = gfit))
  }

  if (!is.null(model)) {
    if (model < 1 | model > 32) {
      stop("Model selected is not on the list of pre-defined models (1,32).")
    }
    gfit <- matrix(NA, ncol = 8, nrow = 1)
    add.row <- mod.list$RepRow[model]
    add.col <- mod.list$RepCol[model]
    add.spl.row <- mod.list$splineRow[model]
    add.spl.col <- mod.list$splineCol[model]
    add.nugget <- mod.list$Nugget[model]
    type.residual <- mod.list$Resid[model]
    mod <- spatial.single(
      data = data, gen = gen, block = block, ibk = ibk, row = row,
      col = col, cov1 = cov1, cov2 = cov2, resp = resp,
      add.block = add.block, add.ibk = add.ibk,
      add.row = add.row, add.col = add.col,
      add.spl.row = add.spl.row, add.spl.col = add.spl.col,
      add.cov1 = add.cov1, add.cov2 = add.cov2,
      add.nugget = add.nugget, type.gen = type.gen,
      type.block = type.block, type.residual = type.residual
    )
    # Storing the statistics for selected model
    # gt <- mod.ref$gt
    # colnames(gfit) <- c('MODEL','n.VC','logL','AIC','BIC','heritPEV','A-opt','D-opt')
    # gfit <- data.frame(gfit)
    # gfit[1,1] <- mod.list$Model[model]
    # gfit[1,2] <- gt$n.vc; gfit[1,3] <- gt$logL; gfit[1,4] <- gt$aic; gfit[1,5] <- gt$bic;
    # gfit[1,6] <- gt$herit.PEV; gfit[1,7] <- gt$Aopt; gfit[1,8] <- gt$Dopt
    # gfit <- data.frame(gfit,mod.list[model,-1])

    # print('Details Model Fitted')
    # print(mod.list[model,])
    # return(list(mod=mod.ref, parms=gfit))
    return(mod)
  }
}


test_lrt <- function(m0 = NULL, m1 = NULL) {
  if (is.null(m0)) {
    return()
  }
  if (is.null(m1)) {
    return()
  }

  likelihood <- try(asreml::lrt.asreml(m0$mod, m1$mod, boundary = FALSE), silent = T)
  if (class(likelihood)[1] == "try-error") {
    dt_m0 <- data.frame(t(unlist(m0$gt)))
    dt_m1 <- data.frame(t(unlist(m1$gt)))
    result <- rbind(dt_m0, dt_m1)[, 1:8] %>% dplyr::mutate_if(is.numeric, round, 3)
  } else {
    result <- likelihood
  }

  return(result)
}
