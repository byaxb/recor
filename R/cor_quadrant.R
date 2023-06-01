#' Fast implementations of (robust) correlation estimators
#'
#' @description
#' Estimate the correlation of two vectors via fast C++ implementations, 
#' with a focus on robust and nonparametric methods.
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for ccaPP::corQuadrant
#'
#' @return correlation coefficients
#' @import ccaPP
#' @export
cor_quadrant <- function(x, y) {
    cor_value <- tryCatch(expr = {
        corQuadrant(x, y)
    }, error = function(err) {NA})
    return(cor_value)
    
}