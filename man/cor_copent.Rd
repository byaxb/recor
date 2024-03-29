% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor_copent.R
\name{cor_copent}
\alias{cor_copent}
\title{Estimating copula entropy}
\usage{
cor_copent(x, y)
}
\arguments{
\item{x, }{numeric vector}

\item{y, }{numeric vector}
}
\value{
The negative value of copula entropy of data x
}
\description{
A wrapper for copent::copent()
}
\details{
See help file for copent::copent
}
\references{
Ma, J., & Sun, Z. (2011). Mutual information is copula entropy. 
Tsinghua Science & Technology, 16(1): 51-54. 
See also arXiv preprint arXiv:0808.0845, 2008.
}
