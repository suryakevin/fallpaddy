#' Export phylogenetic tree to a file
#'
#' @description This function exports a phylogenetic tree as a NEXUS-formatted
#'   file; it is an alias of the `writeNexus` function from the `R` package
#'   `phytools`.
#'
#' @param tree An object of class `phylo`
#' @param file Output file directory
#'
#' @return This function outputs a NEXUS tree file.
#'
#' @author Kevin Surya
#'
#' @import phytools
#'
#' @export
#'
#' @references \href{https://doi.org/10.1111/j.2041-210X.2011.00169.x}{Revell (2012)}
#'
export_tree <- function(tree, file) {
  writeNexus(tree = tree, file = file)
}
