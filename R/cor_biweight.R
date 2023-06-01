#' Tukey's biweight
#'
#' @description
#' A wrapper for biwt::biwt.cor()
#' @usage
#' cor_tic(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help files for biwt::biwt.cor()
#'
#' @return correlation coefficients
#' @import biwt
#' @references 
#' J. Hardin, A. Mitani, L. Hicks, B. VanKoten, 
#' A robust measure of correlation between two genes on a microarray. 
#' BMC Bioinformatics. 8, 220 (2007).
#' @export
cor_biweight <- function(x, y) {
    cor_value <- tryCatch(expr = {
        drop(biwt.cor(rbind(x, y), output="vector"))
    }, error = function(err) {NA})
    return(cor_value)
}
