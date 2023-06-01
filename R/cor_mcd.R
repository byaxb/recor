#' Minimum Covariance Determinant
#'
#' @description
#' A wrapper for robustbase::covMcd()
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' 
#' See help file for robustbase::covMcd
#' 
#' There is cov.mcd() function in MASS,
#' which is slower
#'
#' @return correlation coefficients
#' @import MASS
#' @export
cor_mcd <- function(x, y) {
    cor_value <- tryCatch(expr = {
        cov.mcd(cbind(x, y), cor = TRUE)$cor[1, 2]
    }, error = function(err) {NA})
    return(cor_value)
}
