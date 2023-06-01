#' Robust Copula Dependence - Copula correlation
#'
#' @description
#' A wrapper of rcd::rcd
#' @usage
#' cor_mi(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for rcd::rcd
#'
#' @return correlation coefficients
#' @references 
#' A.A. Ding, Y. Li, Copula Correlation: 
#' An Equitable Dependence Measure and Extension of 
#' Pearson’s Correlation, (2015). http://arxiv.org/abs/1312.7214
#' @import rcd
#' @export
cor_Ccor <- function(x, y) {
    cor_value <- tryCatch(expr = {
        rcd(x, y)
    }, error = function(err) {NA})
    return(cor_value)
}
