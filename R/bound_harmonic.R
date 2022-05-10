#' bound of covariance - harmonic mean of variance X and variance Y
#'
#' @description
#' harmonic mean of variance X and variance Y
#' 
#' @param x, variable x
#' @param y, variable y
#' @details
#' harmonic mean of variance X and variance Y 
#' is defined as 2/(1/var(x) + 1/var(y))
#'
#' @return a numeric vector of length one
#' @export
bound_harmonic <- function(x, y) {
    2/(1/var(x) + 1/var(y))
}