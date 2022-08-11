#' Estimate the average branching rate of every lineage
#'
#' @description This function estimates the average branching rate of every
#'   lineage (node count per time duration) in the phylogenetic tree. This
#'   metric's goal is to gauge the variability in the net speciation and or
#'   duplication rates.
#'
#' @param data A data frame with node count in the 2nd column, time in the 3rd,
#'   and taxon names as the row names
#' @param root_age The age of the tree root; ensure that the time unit is the
#'   same between the root age and the sampling times
#' @param timetree An object of the class `phylo`
#'
#' @return This function returns a vector of the net number of branching events
#'   per unit of time.
#'
#' @author Kevin Surya
#'
#' @import ape
#'
#' @export
#'
#' @references \href{https://doi.org/10.1086/588076}{Freckleton et al. (2008)}
#'
est_branching_rate <- function(data, root_age, timetree) {
  colnames(data)[2] <- "node"
  colnames(data)[3] <- "time"
  data$taxon <- rownames(data)
  if (missing(timetree)) {
    branching_rate <- data$node / (data$time - root_age)
    names(branching_rate) <- data$taxon
  } else if (missing(root_age)) {
    duration <- diag(vcv(phy = timetree))
    branching_rate <- data$node / duration
  }
  return(branching_rate)
}
