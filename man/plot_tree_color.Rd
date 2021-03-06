% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tree.r
\name{plot_tree_color}
\alias{plot_tree_color}
\title{Plot phylogenetic tree with colored branches}
\usage{
plot_tree_color(tree, data, size = 0.3)
}
\arguments{
\item{tree}{An object of class \code{phylo}}

\item{data}{A data frame with taxon name in the row name and the group
assignment in the 1st column}

\item{size}{Branch thickness}
}
\value{
This function returns an object of the \code{ggtree} class.
}
\description{
This function plots a phylogenetic tree with colors
representing groups
}
\author{
Kevin Surya
}
