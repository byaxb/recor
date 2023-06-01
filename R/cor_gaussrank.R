#' Gauss Rank Correlation Estimator
#'
#' @description
#' A wrapper for rococo::gauss.cor()
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for rococo::gauss.cor
#'
#' @return correlation coefficients
#' @import rococo
#' @references 
#' BOUDT K, CORNELISSEN J, CROUX C. 
#' The Gaussian rank correlation estimator: robustness properties[J/OL]. 
#' Statistics and Computing, 2012, 22(2): 471-483. DOI:10.1007/s11222-011-9237-0
#' @export
cor_gaussrank <- function(x, y) {
    cor_value <- tryCatch(expr = {
        x <- as.numeric(x)
        y <- as.numeric(y)
        gauss.cor(x, y)
    }, error = function(err) {NA})
    return(cor_value)
    
}


