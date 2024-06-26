% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{tree_dinosaur_morpho}
\alias{tree_dinosaur_morpho}
\title{Dinosaur phenotypic-scaled tree}
\format{
An object of class \code{phylo} of length 4
}
\source{
\href{https://doi.org/10.1371/journal.pbio.1001853}{Benson et al. (2014)};
\href{https://doi.org/10.1111/pala.12329}{Benson et al. (2017)};
\href{https://doi.org/10.1038/s41559-017-0454-6}{O'Donovan et al. (2018)}
}
\usage{
tree_dinosaur_morpho
}
\description{
This phylogenetic tree, scaled to the amount of body mass
evolution, comprises 402 Mesozoic dinosaur taxa.
}
\details{
\itemize{
\item \strong{Clade:} Dinosauria (Mesozoic dinosaurs)
\item \strong{Number of tips:} 402 dinosaur taxa
\item \strong{Data:} Log10-transformed body mass estimate (kg); species average
\item \strong{Branch length unit:} Original branch length unit (million years)
\eqn{\times} unitless rate scalar \eqn{=} relative amount of body mass
evolution
\item \strong{Inference method:} Variable rates model (\code{BayesTraits})
\item \strong{Rooted?} Yes
\item \strong{Binary or fully-bifurcating?} Yes
}
}
\note{
Regarding the dinosaur timetree, Benson et al. (2014) constructed a
composite cladogram. O'Donovan et al. (2018) acquired this cladogram,
first and last appearance date estimates, and scaled the branch lengths
using the 'mbl' method, enforcing a minimum branch length of one million
years. The tree file is unavailable online, but is available upon request
from the authors.

The timetree from O'Donovan et al. (2018) contains 624 tips. I removed taxa
not present in the body mass dataset. Also, I arbitrarily resolved each
polytomy (or multifurcation) into a series of bifurcations with zero
branch lengths. The resulting bifurcations follow the order in which the
tips appear in the tree.

To scale the dinosaur timetree so that the branch lengths reflect body mass
evolution, I used the variable rates model
\href{https://doi.org/10.1038/nature10516}{(Venditti et al. 2011)} in the
program
\href{http://www.evolution.reading.ac.uk/SoftwareMain.html}{\code{BayesTraits}}.
This model allows the rates of body mass evolution to vary by stretching
and compressing branches during a reversible-jump Markov chain Monte Carlo
(RJMCMC) run. The final tree is a consensus, where each branch length is
the average across the 5,000 MCMC posterior trees.
}
\keyword{datasets}
