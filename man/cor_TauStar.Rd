% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor_TauStar.R
\name{cor_TauStar}
\alias{cor_TauStar}
\title{tau star Statistic of Bergsma and Dassios -- sign covariance}
\usage{
cor_TauStar(x, y)
}
\arguments{
\item{x, }{numeric vector}

\item{y, }{numeric vector}
}
\value{
correlation coefficients
}
\description{
A wrapper for TauStar::tStar()
}
\details{
See help file for TauStar::tStar

Other packages, such as independence::tau.star.test, 
also employ TauStar::tStar() in the background
}
\references{
W. Bergsma, A. Dassios, 
A consistent test of independence based on a sign covariance related to Kendall’s tau. 
Bernoulli. 20 (2014), doi:10.3150/13-BEJ514.
}
