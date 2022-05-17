#' Re-order the dataset to follow the tree
#'
#' @description This function reorders rows in a data frame to follow the order
#'   of tree tip labels
#'
#' @param data A data frame
#' @param tree An object of the class `phylo`
#'
#' @return This function returns a re-ordered data frame.
#'
#' @author Kevin Surya
#'
#' @import ape
#'
#' @export
#'
reorder_data <- function(data, tree) {
  data <- data[match(tree$tip.label, rownames(data)), ]
  return(data)
}
