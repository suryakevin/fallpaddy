% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{tree_mammal_morpho}
\alias{tree_mammal_morpho}
\title{Mammal phenotypic-scaled tree}
\format{
An object of class \code{phylo} of length 4
}
\source{
\href{https://doi.org/10.1073/pnas.1814329116}{Kubo et al. (2019)}
}
\usage{
tree_mammal_morpho
}
\description{
This phylogenetic tree, scaled to the amount of body mass
evolution, comprises 880 terrestrial mammal species.
}
\details{
\itemize{
\item \strong{Clade:} Mammalia
\item \strong{Number of tips:} 880 terrestrial mammal species
\item \strong{Data:} Log10-transformed body mass (kg); species average
\item \strong{Branch length unit:} Original branch length unit (million years)
\eqn{\times} unitless rate scalar \eqn{=} relative amount of body mass
evolution
\item \strong{Inference method:} Variable rates model (\code{BayesTraits})
\item \strong{Rooted?} Yes
\item \strong{Binary or fully-bifurcating?} Yes
}
}
\note{
The original mammal timetree contains polytomies (or multifurcations).
I arbitrarily resolved each polytomy into a series of bifurcations with
zero branch lengths. The resulting bifurcations follow the order in which
the tips appear in the tree.

To scale the mammal timetree so that the branch lengths reflect body mass
evolution, I used the variable rates model
\href{https://doi.org/10.1038/nature10516}{(Venditti et al. 2011)} in the
program
\href{http://www.evolution.reading.ac.uk/SoftwareMain.html}{\code{BayesTraits}}.
This model allows the rates of body mass evolution to vary by stretching
and compressing branches during a reversible-jump Markov chain Monte Carlo
(RJMCMC) run. The final tree is a consensus, where each branch length is
the average across the 10,000 MCMC posterior trees.
}
\keyword{datasets}
