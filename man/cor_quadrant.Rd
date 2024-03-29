% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor_quadrant.R
\name{cor_quadrant}
\alias{cor_quadrant}
\title{Fast implementations of (robust) correlation estimators}
\usage{
cor_quadrant(x, y)
}
\arguments{
\item{x, }{numeric vector}

\item{y, }{numeric vector}
}
\value{
correlation coefficients
}
\description{
Estimate the correlation of two vectors via fast C++ implementations, 
with a focus on robust and nonparametric methods.
}
\details{
See help file for ccaPP::corQuadrant
}
