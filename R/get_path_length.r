#' Extract phylogenetic path lengths
#'
#' @description This function extracts the phylogenetic path lengths or
#'   root-to-tip divergences from a molecular tree or a non-ultrametric
#'   timetree.
#'
#' @param tree An object of class `phylo`
#'
#' @return This function returns a vector of path lengths.
#'
#' @author Kevin Surya
#'
#' @import ape
#'
#' @export
#'
get_path_length <- function(tree) {
  path <- diag(vcv(phy = tree))
  return(path)
}
