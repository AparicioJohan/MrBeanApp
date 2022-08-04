#' Desire Gain Format
#'
#' @param n_traits number of traits
#' @param traits name of the traits
#' @param wt weights
#' @param heritability vector with heritabilities for each trait
#' @param sd vector with genotypic standard deviations
#' @param corPhen matrix with phenotypic correlation
#' @param corGen matrix with genotypic correlation
#'
#' @return data.frame with desire format
#' @export
#'
#' @examples
#' # desireFormat(n_traits = 4)
desireFormat <- function(n_traits = 1,
                         traits = paste0("trait_", 1:n_traits),
                         wt = rep(1, length(traits)),
                         heritability = rep(0.5, length(traits)),
                         sd = rep(1, length(traits)),
                         corPhen = diag(1, length(traits)),
                         corGen = diag(1, length(traits))) {
  if (is.null(corPhen)) corPhen <- corGen
  corPhen <- corPhen
  corPhen[lower.tri(corPhen)] <- NA
  corGen <- corGen
  corGen[upper.tri(corGen)] <- NA
  G <- corPhen
  G[lower.tri(G)] <- corGen[lower.tri(corGen)]
  
  string <-
    c(
      "  Input file for program Desire.",
      "  Number of traits ...  ",
      n_traits,
      "  Trait names ...",
      traits,
      "  Starting economic weights ...",
      wt,
      "  Heritabilities ...",
      heritability,
      "  Standard Deviations ...",
      sd,
      "  Correlation matrix (rp on upper diagonal, rg on lower)."
    )
  
  for (i in 1:nrow(G)) {
    m <- paste(G[i, ], collapse = " ")
    string <- c(string, m)
  }
  
  file <- data.frame(string, row.names = NULL)
  
  return(file)
}