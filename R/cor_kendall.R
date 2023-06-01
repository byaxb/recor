#' Fast implementations of (robust) correlation estimators
#'
#' @description
#' Estimate the correlation of two vectors via fast C++ implementations, 
#' with a focus on robust and nonparametric methods.
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' 
#' See help file for ccaPP::corKendall
#' 
#' Until now (2022-11-14), ccaPP provides the fastest implementation 
#' of Pearson, Spearman, Kendall, Quadrant than 
#' stats::cor, wdm::wdm() 
#' wdm also claims being faster than cor
#'
#' faster even then pcaPP::cor.fk, which claims to be faster 
#' than stats::cor(x, y, method = "Kendall")
#'
#' @return correlation coefficients
#' @import ccaPP
#' @export
cor_kendall <- function(x, y) {
    cor_value <- tryCatch(expr = {
        corKendall(x, y)
    }, error = function(err) {NA})
    return(cor_value)
}