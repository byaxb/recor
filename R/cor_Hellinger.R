#' The Hellinger Correlation
#'
#' @description
#' A wrapper of HellCor::HellCor
#' Empirical value of the Hellinger correlation between two continuous random variables.
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for HellCor::HellCor
#'
#' @return correlation coefficients
#' @references 
#' G. Geenens, P. Lafaye de Micheaux, The Hellinger Correlation. 
#' Journal of the American Statistical Association, 1–15 (2020)
#' @import HellCor
#' @export
cor_Hellinger <- function(x, y) {
    cor_value <- tryCatch(expr = {
        drop(HellCor(x, y)$Hcor)
    }, error = function(err) {NA})
    return(cor_value)
}