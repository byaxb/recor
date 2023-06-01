#' RV coefficient
#'
#' @description
#' A wrapper of FactoMineR::coeffRV
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for FactoMineR::coeffRV
#' 
#' cor_RV(x, y) is actually cor_pearson(x, y)^2 when x and y are one-dimension variable
#' 
#' Obviously, when we calculate RV coefficient by cor_pearson_squared(x, y) 
#' rather than coeffRV(x, y)$rv, it will be much faster.
#'
#' @return correlation coefficients
#' @import FactoMineR
#' @export
cor_RV <- function(x, y) {
    cor_value <- tryCatch(expr = {
        # x <- matrix(x, nrow = length(x))
        # y <- matrix(y, nrow = length(y))
        # coeffRV(x, y)$rv
        cor_pearson_squared(x, y)
    }, error = function(err) {NA})
    return(cor_value)
}


