#' Fast implementations of (robust) correlation estimators
#'
#' @description
#' Estimate the correlation of two vectors via fast C++ implementations, 
#' with a focus on robust and nonparametric methods.
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' Until now (2022-11-14), ccaPP provides the fastest implementation 
#' of Pearson, Spearman, Kendall, Quadrant than 
#' stats::cor, wdm::wdm() 
#' 
#' 
#' wdm also claims being faster than cor
#'
#' @return correlation coefficients
#' @import ccaPP
#' @export
cor_spearman <- function(x, y) {
    cor_value <- tryCatch(expr = {
        corSpearman(x, y)
    }, error = function(err) {NA})
    return(cor_value)
    
}

