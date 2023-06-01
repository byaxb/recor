#' d-variable Hilbert Schmidt independence criterion - dHSIC
#'
#' @description
#' A wrapper for dHSIC::dhsic
#' @usage
#' cor_hsic(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for dHSIC::dhsic
#'
#' @return correlation coefficients
#' @import dHSIC
#' @export
cor_dhsic <- function(x, y) {
    cor_value <- tryCatch(expr = {
        drop(dhsic(list(x, y))$dHSIC)
    }, error = function(err) {NA})
    return(cor_value)
}