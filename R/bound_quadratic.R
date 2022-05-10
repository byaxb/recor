#' bound of covariance - quadratic mean of variance X and variance Y
#'
#' @description
#' quadratic mean of variance X and variance Y
#' 
#' @param x, variable x
#' @param y, variable y
#' @details
#' quadratic mean of variance X and variance Y 
#' is defined as sqrt((var(x)^2+var(y)^2)/2)
#'
#' @return a numeric vector of length one
#' @export
bound_quadratic <- function(x, y) {
    sqrt((var(x)^2+var(y)^2)/2)
}