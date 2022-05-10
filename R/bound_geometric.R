#' bound of covariance - geometric mean of variance X and variance Y
#'
#' @description
#' geometric mean of variance X and variance Y
#' 
#' @param x, variable x
#' @param y, variable y
#' @details
#' geometric mean of variance X and variance Y 
#' is defined as sqrt(var(x)*var(y))
#'
#' @return a numeric vector of length one
#' @export
bound_geometric <- function(x, y) {
    sqrt(var(x)*var(y))
}