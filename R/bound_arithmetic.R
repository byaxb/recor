#' bound of covariance - arithmetic mean of variance X and variance Y
#'
#' @description
#' arithmetic mean of variance X and variance Y
#' 
#' @param x, variable x
#' @param y, variable y
#' @details
#' arithmetic mean of variance X and variance Y 
#' is defined as (var(x)+var(y)) / 2
#'
#'
#' @return a numeric vector of length one
#' @export
bound_arithmetic <- function(x, y) {
    (var(x)+var(y)) / 2
}