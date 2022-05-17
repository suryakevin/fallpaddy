#' Decompose the variance-covariance matrix
#'
#' @description This function decomposes the phylogenetic variance-covariance
#'   matrix into its correlation (off-diagonal) and variance components
#'   (on-diagonal). The output from `decomp_vcv` is necessary for using the
#'   `est_node_density` and `fit_punc_model` functions.
#'
#' @param tree An object of class `phylo`
#' @param lambda Pagel's \eqn{\lambda}, a measure of phylogenetic signal
#' @param est_lambda If `TRUE`, the downstream function(s) estimate Pagel's
#'   \eqn{\lambda}
#'
#' @return This function returns a list comprising a correlation matrix (a
#'   `corPagel` object) and a variance weight vector.
#'
#' @author Kevin Surya
#'
#' @import ape
#'
#' @export
#'
#' @references \href{https://doi.org/10.1038/44766}{Pagel (1999)};
#'   \href{https://doi.org/10.1086/343873}{Freckleton et al. (2002)};
#'   \href{https://doi.org/10.1093/bioinformatics/bty633}{Paradis and Schliep (2019)}
#'
decomp_vcv <- function(tree, lambda = 1, est_lambda = FALSE) {
  taxon <- tree$tip.label
  corr <- corPagel(
    value = lambda,
    phy = tree,
    form = ~taxon,
    fixed = !est_lambda
  )
  w <- diag(vcv(phy = tree))
  vcv_parts <- list(corr, w)
  names(vcv_parts) <- c("corr", "w")
  return(vcv_parts)
}
