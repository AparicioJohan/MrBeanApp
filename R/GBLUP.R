#' GBLUP and rrBLUP estimation
#'
#' @param pheno_data data.frame with phenotypic data
#' @param geno_matrix data.frame with first column identifying the genotype names
#'  and the rest containing marker information in format (-1, 0 , 1).
#' @param genoype string with the genotype name
#' @param traits vector with the traits to be analyzed
#' @param method "GBLUP", "rrBLUP" or "mix" ("mix" in progress)
#'
#' @return list with GBLUPs, variance components and marker effects.
#'  list(results, var_comp, markers)
#'
#' @export
#'
#' @examples
#' # GBLUPs(
#' # pheno_data = NULL,
#' # geno_matrix = NULL,
#' # genoype = NULL,
#' # traits = NULL,
#' # method = c("GBLUP", "rrBLUP")
#' # )
GBLUPs <- function(pheno_data = NULL,
                   geno_matrix = NULL,
                   genotype = NULL,
                   traits = NULL,
                   method = c("GBLUP", "rrBLUP", "mix")) {
  pheno_data <- data.frame(pheno_data, row.names = NULL) %>%
    dplyr::distinct(.data[[genotype]], .keep_all = TRUE) %>%
    dplyr::mutate(level = as.factor(.data[[genotype]])) %>%
    tibble::column_to_rownames(genotype) %>%
    dplyr::relocate(level) %>%
    droplevels()

  rows <- geno_matrix[, 1]
  cols <- colnames(geno_matrix)[-1]
  geno_matrix <- as.matrix(geno_matrix[, -1])
  rownames(geno_matrix) <- rows
  colnames(geno_matrix) <- cols

  check_matrix <- inherits(geno_matrix, what = "matrix")
  if (!check_matrix) {
    stop("Check your genotypic data")
  }
  missing_values <- sum(is.na(geno_matrix)) >= 1
  if (missing_values) {
    stop("Missing values in the genotypic matrix")
  }
  lvls <- data.frame(table(geno_matrix))$geno_matrix
  condition <- sum(lvls %in% c("-1", "0", "1"))
  if (condition <= 1) {
    stop("The genotypic matrix has to be in (-1, 0, 1) format")
  }

  shared <- data.frame(
    trait = as.character(),
    pheno = as.numeric(),
    geno = as.numeric(),
    shared = as.numeric()
  )
  gen_in_common <- list()
  for (i in traits) {
    lines_phen <- as.character(pheno_data[!is.na(pheno_data[, i]), "level"])
    lines_gen <- as.character(rownames(geno_matrix))
    gen_in_common[[i]] <- intersect(lines_phen, lines_gen)
    tsh <- data.frame(
      trait = i,
      pheno = length(lines_phen),
      geno = length(lines_gen),
      shared = length(gen_in_common[[i]])
    )
    shared <- rbind(shared, tsh)
  }
  cat("\n")
  print(shared, row.names = FALSE)
  cat("\n")

  pheno_data <- pheno_data %>%
    subset(level %in% rownames(geno_matrix)) %>%
    droplevels()

  gblups_results <- pheno_data %>% dplyr::select(level)
  rrblups_results <- pheno_data %>% dplyr::select(level)
  mix_gblups_results <- pheno_data %>% dplyr::select(level)

  if ("GBLUP" %in% method) {
    var_comp <- data.frame(trait = traits, var_g = NA, var_e = NA, h2 = NA)
    K <- sommer::A.mat(geno_matrix)
    colnames(K) <- rownames(K) <- rownames(geno_matrix)
    for (var in traits) {
      equation <- reformulate("1", response = var)
      GBLUP <- sommer::mmer(
        equation,
        random = ~ sommer::vsr(level, Gu = K),
        rcov = ~units,
        getPEV = FALSE,
        data = pheno_data,
        verbose = FALSE
      )
      id <- names(GBLUP$U$`u:level`[[1]])
      gblups_results[id, var] <- GBLUP$U$`u:level`[[1]] + GBLUP$Beta$Estimate

      var_g <- GBLUP$sigma$`u:level`
      var_e <- GBLUP$sigma$units
      var_comp[var_comp$trait == var, "var_g"] <- var_g
      var_comp[var_comp$trait == var, "var_e"] <- var_e
      var_comp[var_comp$trait == var, "h2"] <- var_g / (var_g + var_e)
    }
    gblups_results <- gblups_results %>%
      dplyr::mutate(
        phenotypic = ifelse(!is.na(level), TRUE, FALSE),
        level = rownames(.)
      ) %>%
      dplyr::relocate(level, phenotypic)
    var_comp <- merge(shared, var_comp, by = "trait")
    names(var_comp) <- c(
      "Trait", "Pheno", "Geno", "Shared", "VarG", "VarE", "Genomic_h2"
    )
  } else {
    var_comp <- NULL
    gblups_results <- NULL
  }

  if ("rrBLUP" %in% method) {
    marker_effects <- data.frame(
      marker = colnames(geno_matrix), row.names = colnames(geno_matrix)
    )
    for (var in traits) {
      trn_geno_matrix <- geno_matrix[gen_in_common[[var]], ]
      equation <- reformulate("1", response = var)
      rrBLUP <- sommer::mmer(
        equation,
        random = ~ sommer::vsr(list(trn_geno_matrix), buildGu = FALSE),
        rcov = ~units,
        getPEV = FALSE,
        nIters = 3,
        data = pheno_data,
        verbose = FALSE
      )
      markers <- rrBLUP$U$`u:trn_geno_matrix`[[1]]
      u <- geno_matrix %*% as.matrix(markers)
      rownames(u) <- rownames(geno_matrix)
      rrblups_results[rownames(u), var] <- u + rrBLUP$Beta$Estimate
      marker_effects[names(markers), var] <- markers
    }
    rrblups_results <- rrblups_results %>%
      dplyr::mutate(
        phenotypic = ifelse(!is.na(level), TRUE, FALSE),
        level = rownames(.)
      ) %>%
      dplyr::relocate(level, phenotypic)
  } else {
    marker_effects <- NULL
    rrblups_results <- NULL
    marker_effects <- NULL
  }

  if ("mix" %in% method) {
    var_comp_mix <- data.frame(trait = traits, var_g = NA, var_e = NA, h2 = NA)
    marker_effects_mix <- data.frame(
      marker = colnames(geno_matrix), row.names = colnames(geno_matrix)
    )
    M <- geno_matrix
    MMT <- tcrossprod(M)
    colnames(MMT) <- rownames(MMT) <- rownames(geno_matrix)
    MMT_inv <- solve(MMT + diag(1e-6, ncol(MMT), ncol(MMT)))
    MT_MMT_inv <- t(M) %*% MMT_inv
    adj <- adj_vanraden(M)
    for (var in traits) {
      n <- sum(!is.na(pheno_data[[var]]))
      k <- 1
      equation <- reformulate("1", response = var)
      mixGBLUP <- sommer::mmer(
        equation,
        random = ~ sommer::vsr(level, Gu = MMT),
        rcov = ~units,
        verbose = FALSE,
        data = pheno_data
      )
      gblup <- mixGBLUP$U$`u:level`[[1]]
      markers <- MT_MMT_inv %*% matrix(gblup, ncol = 1)
      var_g <- kronecker(MMT, mixGBLUP$sigma$`u:level`) - mixGBLUP$PevU$`u:level`[[1]]
      var_markers <- t(M) %*% MMT_inv %*% (var_g) %*% t(MMT_inv) %*% M
      se_markers <- sqrt(diag(var_markers))
      t_stat_from_g <- markers / se_markers
      pval_GBLUP <- dt(t_stat_from_g, df = n - k - 1)

      id <- names(gblup)
      intercept <- mixGBLUP$Beta$Estimate
      mix_gblups_results[id, var] <- gblup + intercept

      var_g <- mixGBLUP$sigma$`u:level` * adj
      var_e <- mixGBLUP$sigma$units
      var_comp_mix[var_comp_mix$trait == var, "var_g"] <- var_g
      var_comp_mix[var_comp_mix$trait == var, "var_e"] <- var_e
      var_comp_mix[var_comp_mix$trait == var, "h2"] <- var_g / (var_g + var_e)

      marker_effects_mix[rownames(markers), var] <- markers
      marker_effects_mix[rownames(markers), paste0("pvalue_", var)] <- pval_GBLUP
    }
    mix_gblups_results <- mix_gblups_results %>%
      dplyr::mutate(
        phenotypic = ifelse(!is.na(level), TRUE, FALSE),
        level = rownames(.)
      ) %>%
      dplyr::relocate(level, phenotypic)
    var_comp_mix <- merge(shared, var_comp_mix, by = "trait")
    names(var_comp_mix) <- c(
      "Trait", "Pheno", "Geno", "Shared", "VarG", "VarE", "Genomic_h2"
    )
  } else {
    var_comp_mix <- NULL
    mix_gblups_results <- NULL
    marker_effects_mix <- NULL
  }

  results <- list(
    "GBLUP" = gblups_results,
    "rrBLUP" = rrblups_results,
    "mixGBLUP" = mix_gblups_results
  )

  info_markers <- list(
    "rrBLUP" = marker_effects,
    "mixGBLUP" = marker_effects_mix
  )

  variance_comp <- list(
    "GBLUP" = var_comp,
    "mixGBLUP" = var_comp_mix
  )

  return(
    list(
      results = results,
      var_comp = variance_comp,
      markers = info_markers
    )
  )
}


#' Adjusted Value
#'
#' @param geno_matrix matrix n_gen by n_marker dimension in format (-1, 0 , 1)
#'
#' @return adjusted value van Raden
#' @noRd
adj_vanraden <- function(geno_matrix) {
  snps <- geno_matrix + 1
  fa <- colSums(snps) / (2 * nrow(snps))
  index_non <- fa >= 1 | fa <= 0
  snps <- snps[, !index_non]
  n_Ind <- nrow(snps)
  n <- n_Ind
  p <- colSums(snps) / (2 * n_Ind)
  adj <- 2 * sum(p * (1 - p))
  return(adj)
}
