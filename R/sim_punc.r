#' Simulate a phylogenetic tree with a specified signature of punctuated
#'   evolution
#'
#' @description This function first simulates a birth-death timetree
#'   (extant-only, serially-sampled, or extinct only) and then the
#'   corresponding molecular or morphological tree that bears the
#'   user-specified signature of punctuated evolution (i.e., the speciational
#'   contribution to the total divergence).
#'
#' @details In this simulation, as we turn the knob, increasing the
#'   speciational contribution to the total divergence, the molecular or
#'   morphological evolution becomes less clock-like, and sampling time
#'   predicts divergence less and less. This simulation, however, cannot create
#'   deviations from the strict clock unrelated to punctuation (e.g.,
#'   single-branch or clade-wide rate shifts).
#'
#' @param n Number of tips in the tree
#' @param age The total depth of the timetree; the unit is up to the user
#'   (e.g., `age = 100` could mean that the age of the most recent common
#'   ancestor is 100 years or 100 million years ago)
#' @param rate The maximum net evolutionary rate; the unit is up to the user
#'   (e.g., `rate = 0.001` could mean 0.001 DNA substitutions per site per year
#'   or 0.001 kilograms per million year)
#' @param punc The proportion of total tree length attributable to
#'   punctuational effects (ranges from 0 \[completely gradual\] to 1
#'   \[completely punctuational\])
#' @param sampling List of options:
#'   \itemize{
#'     \item "extant": all taxa in the dataset are sampled at the "same" time
#'       (e.g., present-day mammal species)
#'     \item "serial_fossil": taxa are serially-sampled (e.g., SARS-CoV-2
#'       genomes and extinct dinosaur species)
#'   }
#'
#' @return This function returns a list comprising a pair of simulated trees: a
#'   timetree and its corresponding molecular/morphological tree.
#'
#' @author Kevin Surya
#'
#' @import ape motmot phytools
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @references \href{https://doi.org/10.1214/aoms/1177730285}{Kendall (1948)};
#'   \href{https://doi.org/10.1038/44766}{Pagel (1999)};
#'   \href{https://doi.org/10.1126/science.1129647}{Pagel et al. (2006)};
#'   \href{https://doi.org/10.1093/sysbio/syr029}{Stadler (2011)};
#'   \href{https://doi.org/10.1016/j.jtbi.2015.09.005}{Paradis (2015)};
#'   \href{https://doi.org/10.3389/fviro.2023.1066147}{Surya et al. (2023)}
#'
sim_punc <- function(
  n,
  age,
  rate,
  punc,
  sampling = c("extant", "serial_fossil")
) {
  if (sampling == "extant") {
    # simulates ultrametric timetree
    birth <- runif(n = 1, min = 0, max = 5)
    death <- runif(n = 1, min = 0, max = birth)
    tree_time <- rphylo(n = n, birth = birth, death = death, fossils = FALSE)
    # codes below adapted from http://blog.phytools.org/2012/02/quicker-way-to-rescale-total-length-of.html
    tree_time$edge.length <-
      (tree_time$edge.length / max(nodeHeights(tree = tree_time)[, 2])) * age
    # simulates molecular/morphological tree
    kappa <- 0.9919699 + (-1.5655721 * punc) + (1.2208072 * punc^2) +
             (-0.6489358 * punc^3)  # see Surya et al. (202X)
      # as 'punc' approaches zero, 'kappa' approaches one
      #   (no change in the timetree)
    tree_evol <- transformPhylo(phy = tree_time, model = "kappa", kappa = kappa)
    max_net_evol <- age * rate
    tree_evol$edge.length <-
      (tree_evol$edge.length / max(nodeHeights(tree = tree_evol))) *
      max_net_evol
    # organizes outputs
    sim_trees <- list(tree_time, tree_evol)
    names(sim_trees) <- c("time", "evol")
    return(sim_trees)
  } else {
    # simulates non-ultrametric timetree
    birth <- runif(n = 1, min = 1, max = 5)
    death <- birth - runif(n = 1, min = 0.01, max = 0.15)  # to mimic empirical trees
    tree_time <- rphylo(n = n, birth = birth, death = death, fossils = TRUE)
    if (length(tree_time$tip.label) > n) {
      tip_keep <- sample(x = tree_time$tip.label, size = n)
      tree_time <- keep.tip(phy = tree_time, tip = tip_keep)
    }
    tree_time$edge.length <-
      (tree_time$edge.length / max(nodeHeights(tree = tree_time)[, 2])) * age
    # simulates molecular/morphological tree
    kappa <- 0.9976766 + (-1.4403441 * punc) + (0.8569546 * punc^2) +
             (-0.4144971 * punc^3)
    tree_evol <- transformPhylo(phy = tree_time, model = "kappa", kappa = kappa)
    max_net_evol <- age * rate
    tree_evol$edge.length <-
      (tree_evol$edge.length / max(nodeHeights(tree = tree_evol))) *
      max_net_evol
    # organizes outputs
    sim_trees <- list(tree_time, tree_evol)
    names(sim_trees) <- c("time", "evol")
    return(sim_trees)
  }
}
