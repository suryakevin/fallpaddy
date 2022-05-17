#' Estimate the punctuational contribution to the total divergence
#'
#' @description This function estimates how much of the total divergence,
#'   molecular or morphological, is attributable to speciation or gene
#'   duplication events.
#'
#' @param tree An object of class `phylo`
#' @param output_reg A list outputted from the `fit_punc_model` function
#'
#' @return This function returns the punctuational contribution value.
#'
#' @author Kevin Surya
#'
#' @import ape
#'
#' @export
#'
#' @references \href{https://doi.org/10.1126/science.1129647}{Pagel et al. (2006)}
#'
est_punc_contrib <- function(tree, output_reg) {
  model <- output_reg$model
  if (model$dims$p == 2) {  # path ~ node
    beta <- model$coefficients[2]  # punctuational contribution at each node
  } else if (model$dims$p == 3) {  # path ~ time + node
    beta <- model$coefficients[3]  # punctuational contribution at each node
  }
  s <- length(tree$tip.label)
  T <- sum(tree$edge.length)
  punc_contrib <- as.numeric((2 * (s - 1) * beta) / T)
  # 2(s-1) is the number of branches in a fully-bifurcating tree
  return(punc_contrib)
}
