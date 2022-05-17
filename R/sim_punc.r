#' Simulate a phylogenetic tree with a specified signature of punctuated
#'   evolution
#'
#' @description This function first simulates a birth-death timetree
#'   (extant-only, serially-sampled, or extinct only) and then the
#'   corresponding molecular or morphological tree that bears the
#'   user-specified signature of punctuated evolution (i.e., the speciational
#'   contribution to the total divergence).
#'
#' @details In this simulation, the default is that evolution follows a strict
#'   clock. As we turn the knob, increasing the speciational contribution to
#'   the total divergence, the molecular or morphological evolution becomes
#'   less clock-like, and sampling time predicts divergence less and less.
#'   Therefore, the range of scenarios that this simulation can create is still
#'   limited. For example, this simulation cannot create deviations from the
#'   strict clock unrelated to punctuation (e.g., single-branch or clade-wide
#'   rate shifts).
#'
#' @param n Number of tips in the tree
#' @param age The total depth of the timetree; the unit is up to the user
#'   (e.g., `age = 100` could mean that the age of the most recent common
#'   ancestor is 100 years or 100 million years ago)
#' @param rate The maximum net evolutionary rate; the unit is up to the user
#'   (e.g., `rate = 0.001` could mean 0.001 DNA substitutions per site per year
#'   or 0.001 kilograms per million year)
#' @param punc The proportion of total tree length attributable to
#'   punctuational effects (ranges from 0 [completely gradual] to 1
#'   [completely punctuational])
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
#' @export
#'
#' @references \href{https://doi.org/10.1214/aoms/1177730285}{Kendall (1948)};
#'   \href{https://doi.org/10.1038/44766}{Pagel (1999)};
#'   \href{https://doi.org/10.1126/science.1129647}{Pagel et al. (2006)};
#'   \href{https://doi.org/10.1093/sysbio/syr029}{Stadler (2011)};
#'   \href{10.1016/j.jtbi.2015.09.005}{Paradis (2015)}
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
    kappa <- 0.9917222 + (-1.5558826 * punc) + (1.1931311 * punc^2) +
             (-0.6306910 * punc^3)  # see scribbles.r
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
    death <- birth - 0.15
    tree_time <- rphylo(n = n, birth = birth, death = death, fossils = TRUE)
    root_tip <- diag(vcv(phy = tree_time))
    extant_keep <- names(root_tip[root_tip < max(root_tip)])
    tree_time <- keep.tip(phy = tree_time, tip = extant_keep)
    if (length(tree_time$tip.label) > 100) {
      tip_keep <- sample(x = tree_time$tip.label, size = 100)
      tree_time <- keep.tip(phy = tree_time, tip = tip_keep)
    }
    tree_time$edge.length <-
      (tree_time$edge.length / max(nodeHeights(tree = tree_time)[, 2])) * age
    # simulates molecular/morphological tree
    kappa <- 0.9978070 + (-1.4586438 * punc) + (0.8925897 * punc^2) +
             (-0.4323218 * punc^3)
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
