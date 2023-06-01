#' Distance Correlation Coefficient
#'
#' @description
#' A wrapper for energy::dcor
#' @usage
#' cor_dcor(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' 
#' More efficient dcor2d are used
#' 
#' Since dcor2d(x, y) = dcor(x, y)^2, its square root is adpoted.
#' 
#' @return correlation coefficients
#' @import dcov
#' @export
cor_dcor <- function(x, y) {
    cor_value <- tryCatch(expr = {
        drop(sqrt(dcor2d(x, y)))
    }, error = function(err) {NA})
    return(cor_value)
    
}