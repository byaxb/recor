#' tau star Statistic of Bergsma and Dassios -- sign covariance
#'
#' @description
#' A wrapper for TauStar::tStar()
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for TauStar::tStar
#' 
#' Other packages, such as independence::tau.star.test, 
#' also employ TauStar::tStar() in the background 
#'
#' @return correlation coefficients
#' @import TauStar
#' @references 
#' W. Bergsma, A. Dassios, 
#' A consistent test of independence based on a sign covariance related to Kendall’s tau. 
#' Bernoulli. 20 (2014), doi:10.3150/13-BEJ514.
#' 
#' @export
cor_TauStar <- function(x, y) {
    cor_value <- tryCatch(expr = {
        tStar(x, y)
    }, error = function(err) {NA})
    return(cor_value)
}
