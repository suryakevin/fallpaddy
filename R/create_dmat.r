#' Create the normalizing matrix \eqn{D}
#'
#' @description This function creates the normalizing matrix \eqn{D} by taking
#'   the Cholesky decomposition of the inverse of the phylogenetic
#'   variance-covariance matrix. This process is called Cholesky whitening. As
#'   far as I know, it requires fewer matrix calculations than some methods
#'   that involve singular value decomposition (SVD).
#'
#' @param tree An object of class `phylo`
#'
#' @return This function returns a \eqn{D} matrix.
#'
#' @author Kevin Surya
#'
#' @export
#'
#' @references \href{https://doi.org/10.1080/00031305.2016.1277159}{Kessy et al. (2018)}
#'
create_dmat <- function(tree) {
  D <- chol(solve(vcv(phy = tree)))
  return(D)
}
