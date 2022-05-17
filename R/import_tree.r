#' Import phylogenetic tree (NEXUS format)
#'
#' @description This function imports a phylogenetic tree file written in the
#'   NEXUS format; it is an alias of the `read.nexus` function from the `R`
#'   package `ape`.
#'
#' @param file Input file directory
#'
#' @return This function returns an object of class `phylo`.
#'
#' @author Kevin Surya
#'
#' @import ape
#'
#' @export
#'
#' @references \href{https://doi.org/10.1093/bioinformatics/bty633}{Paradis and Schliep (2019)}
#'
import_tree_nex <- function(file) {
  tree <- read.nexus(file = file)
  return(tree)
}

#' Import phylogenetic tree (Newick format)
#'
#' @description This function imports a phylogenetic tree file written in the
#'   Newick format; it is an alias of the `read.newick` function from the `R`
#'   package `phytools`.
#'
#' @param file Input file directory
#'
#' @return This function returns an object of class `phylo`.
#'
#' @author Kevin Surya
#'
#' @import phytools
#'
#' @export
#'
#' @references \href{https://doi.org/10.1111/j.2041-210X.2011.00169.x}{Revell (2012)}
#'
import_tree_nwk <- function(file) {
  tree <- read.newick(file = file)
  return(tree)
}
