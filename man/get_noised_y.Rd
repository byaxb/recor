% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_noised_y.R
\name{get_noised_y}
\alias{get_noised_y}
\title{Get noised y}
\usage{
get_noised_y(y_hat, SNR = 1, dB = NULL)
}
\arguments{
\item{y_hat, }{signal}

\item{SNR, }{signal-to-noise rate}
}
\value{
power, scalar value
}
\description{
Get noised y with certain SNR
}
\details{
The noised y is calculated in the following way:
y = y_hat + noise

Either SNR or dB should be set to control the noise level.
}
