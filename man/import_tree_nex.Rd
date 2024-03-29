% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_tree.r
\name{import_tree_nex}
\alias{import_tree_nex}
\title{Import phylogenetic tree (NEXUS format)}
\usage{
import_tree_nex(file)
}
\arguments{
\item{file}{Input file directory}
}
\value{
This function returns an object of class \code{phylo}.
}
\description{
This function imports a phylogenetic tree file written in the
NEXUS format; it is an alias of the \code{read.nexus} function from the \code{R}
package \code{ape}.
}
\references{
\href{https://doi.org/10.1093/bioinformatics/bty633}{Paradis and Schliep (2019)}
}
\author{
Kevin Surya
}
