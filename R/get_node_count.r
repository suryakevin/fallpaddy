#' Extract node counts
#'
#' @description This function extracts the cumulative number of internal nodes
#'   along the root-to-tip paths.
#'
#' @param tree An object of class `phylo`
#'
#' @return This function returns a vector of node counts.
#'
#' @author Kevin Surya
#'
#' @import ape
#'
#' @export
#'
get_node_count <- function(tree) {
  node <- unlist(lapply(nodepath(phy = tree), function(x) {length(x) - 2}))
  names(node) <- tree$tip.label
  return(node)
}
