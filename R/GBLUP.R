#' GBLUP and rrBLUP estimation
#'
#' @param pheno_data data.frame with phenotypic data
#' @param geno_matrix data.frame with first column identifying the genotype names
#'  and the rest containing marker information in format (-1, 0 , 1).
#' @param genoype string with the genotype name
#' @param traits vector with the traits to be analyzed
#' @param method "GBLUP", "rrBLUP" or "mix" ("mix" in progress)
#'
#' @return List with GBLUPs, variance components and marker effects.
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
  
  gblups_results <- data.frame(NULL)
  rrblups_results <- pheno_data %>% dplyr::select(level)
  mix_gblups_results <- pheno_data %>% dplyr::select(level)
  
  if ("GBLUP" %in% method) {
    var_comp <- data.frame(trait = traits, var_g = NA, var_e = NA, h2 = NA)
    tmp_list <- list()
    K <- sommer::A.mat(geno_matrix)
    colnames(K) <- rownames(K) <- rownames(geno_matrix)
    for (var in traits) {
      equation <- reformulate("1", response = var)
      GBLUP <- sommer::mmer(
        equation,
        random = ~ sommer::vsr(level, Gu = K),
        rcov = ~units,
        getPEV = TRUE,
        data = pheno_data,
        verbose = FALSE
      )
      
      var_g <- GBLUP$sigma$`u:level`
      var_e <- GBLUP$sigma$units
      var_comp[var_comp$trait == var, "var_g"] <- var_g
      var_comp[var_comp$trait == var, "var_e"] <- var_e
      var_comp[var_comp$trait == var, "h2"] <- var_g / (var_g + var_e)
      
      coefficients <- GBLUP$U$`u:level`[[1]]
      id <- names(coefficients)
      gblups_results[id, "trait"] <- var
      gblups_results[, "level"] <- id
      gblups_results[gen_in_common[[var]], "type"] <- "fit"
      gblups_results <- gblups_results %>% 
        dplyr::mutate(type = ifelse(is.na(type), "prediction", type))
      
      intercept <- GBLUP$Beta$Estimate
      PEV <- diag(GBLUP$PevU$`u:level`[[1]])
      id_phen <- as.character(pheno_data[, "level"])
      gblups_results[id_phen, "observed"] <- pheno_data[, var]
      gblups_results[, "predicted.value"] <- coefficients + intercept
      gblups_results[, "GEBVs"] <- coefficients
      gblups_results[, "standard.error"] <- sqrt(PEV)
      gblups_results[, "reliability"] <- 1 - PEV / c(var_g)
      corr <- cor(
        x = gblups_results$observed,
        y = gblups_results$predicted.value, 
        use = "pairwise.complete.obs"
      )
      var_comp[var_comp$trait == var, "Corr"] <- corr
      # gblups_results[, "z.ratio"] <- coefficients / sqrt(PEV)
      # gblups_results[, "PEV"] <- PEV
      tmp_list[[var]] <- gblups_results
      gblups_results <- data.frame(NULL)
    }
    gblups_results <- data.frame(dplyr::bind_rows(tmp_list), row.names = NULL)
    var_comp <- merge(shared, var_comp, by = "trait")
    names(var_comp) <- c(
      "Trait", "Pheno", "Geno", "Shared", "VarG", "VarE", "Genomic_h2", "Corr"
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


#' Marker Plot
#'
#' @param marker data.frame with marker effects (marker, trait_1, trait_2, ...).
#' @param map data.frame with the genetic map (marker, position, chr).
#' @param trait_selected vector with traits to plot
#' @param type string selecting whether to use points or lines in the geom.
#'  (point by default).
#' @param legend_size numeric value to define the size of the theme 
#' (15 by default).
#' @param alpha numeric value between (0, 1) to define the alpha in the geom.
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' # marker_plot(
#' # marker = marker_info,
#' # map = map,
#' # trait_selected = traits,
#' # type = "line",
#' # alpha = 1
#' # )
marker_plot <- function(marker = NULL,
                        map = NULL,
                        trait_selected = "",
                        type = "point",
                        legend_size = 15,
                        alpha = 1) {
  lvls <- colnames(map)
  if (length(lvls) > 3) {
    stop("Check the format of the genetic map")
  }
  required_names <- c("marker", "position", "chr")
  check <- which(names(map) %in% required_names)
  if (length(check) < 3) {
    stop("Column names; 'marker', 'position' and 'chr' need \n
         to be present in the data frame provided.",
         call. = FALSE
    )
  }
  num_chr <- dplyr::n_distinct(map$chr)
  if (num_chr == 1) {
    table_dt <- marker %>%
      tidyr::gather(key = "trait", value = "value", -1) %>%
      dplyr::filter(trait %in% trait_selected)
    mark_plot <- table_dt %>%
      ggplot(
        aes(
          x = marker,
          y = value^2
        )
      ) +
      {
        if (type == "point") {
          geom_point(alpha = alpha)
        } else if (type == "line") {
          geom_segment(
            aes(
              x = marker,
              xend = marker,
              y = 0,
              yend = value^2
            ),
            alpha = alpha
          )
        }
      } +
      theme(
        text = element_text(size = legend_size),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(
          fill = "white",
          colour = "white"
        )
      ) +
      labs(
        x = "Marker",
        y = "Estimated Squared-Marker Effect"
      ) +
      facet_wrap(~trait, nrow = length(trait_selected), scales = "free_y")
  } else if (num_chr > 1) {
    marker_id <- colnames(marker)[1]
    colnames(map)[1] <- marker_id
    marker_info <- merge(map, marker, by = marker_id, sort = FALSE)
    marker_info <- marker_info %>%
      dplyr::group_by(chr) %>%
      dplyr::summarise(chr_len = max(position)) %>%
      dplyr::mutate(tot = cumsum(chr_len) - chr_len) %>%
      dplyr::select(-chr_len) %>%
      dplyr::left_join(marker_info, ., by = c("chr" = "chr")) %>%
      dplyr::arrange(chr, position) %>%
      dplyr::mutate(BPcum = position + tot) %>%
      dplyr::relocate(marker, position, chr, tot, BPcum)
    axisdf <- marker_info %>%
      dplyr::group_by(chr) %>%
      dplyr::summarize(center = (max(BPcum) + min(BPcum)) / 2)
    mark_plot <- marker_info %>%
      tidyr::gather(key = "trait", value = "value", -(1:5)) %>%
      dplyr::filter(trait %in% trait_selected) %>%
      ggplot(
        aes(
          x = BPcum,
          y = value^2,
          color = chr
        )
      ) +
      {
        if (type == "point") {
          geom_point(alpha = alpha)
        } else if (type == "line") {
          geom_line(alpha = alpha)
        }
      } +
      theme(
        text = element_text(size = legend_size),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(
          fill = "white",
          colour = "white"
        )
      ) +
      labs(
        x = "Chr",
        y = "Estimated Squared-Marker Effect"
      ) +
      facet_wrap(~trait, nrow = length(trait_selected), scales = "free_y") +
      scale_x_continuous(label = axisdf$chr, breaks = axisdf$center)
  }
  return(mark_plot)
}
