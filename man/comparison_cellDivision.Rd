% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison.R
\name{comparison_cellDivision}
\alias{comparison_cellDivision}
\title{Split a reference scan into a grid of cells}
\arguments{
\item{x3p}{an x3p object containing a breech face scan}

\item{numCells}{a vector of two numbers representing the number of cells along
the row and column dimensions into which the x3p is partitioned}
}
\value{
A tibble containing a prod(numCells) number of rows. Each row contains
a single cell's index of the form (row #, col #) and an x3p object
containing the breech face scan of that cell.
}
\description{
Split a reference scan into a grid of cells
}
\examples{
data(fadul1.1_processed)

cellTibble <- fadul1.1_processed \%>\%
comparison_cellDivision(numCells = c(8,8))

head(cellTibble)

}
