#' Mutual Information
#'
#' @description
#' Mutual Information Dependence Measure
#' @usage
#' cor_mi(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' There are several packages for MI, we adopt JMI here.
#' 
#' See JMI::JMI for more details.
#' 
#' \eqn{sqrt(1-exp(-2*mi))} is returned.
#'
#' @return correlation coefficients
#' @import JMI
#' @export
cor_mi <- function(x, y) {
    cor_value <- tryCatch(expr = {
        mi <- JMI(x, y, BN = 0)$mi
        drop(sqrt(1-exp(-2*mi)))
    }, error = function(err) {NA})
    return(cor_value)
}
