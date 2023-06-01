#' CHSIC
#' @description
#' codes from github, see details
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' Codes from  
#' https://github.com/stochasticresearch/copulastatistic
#' 
#' which is further extracted from 
#' 
#' Code from Lopez-Paz 
#' https://github.com/lopezpaz/randomized_dependence_coefficient
#'
#' @return correlation coefficients
#' @export
cor_hsiccop <- function(x, y) {
    cor_value <- tryCatch(expr = {
        x <- apply(as.matrix(x),2,function(u) ecdf(u)(u))
        y <- apply(as.matrix(y),2,function(u) ecdf(u)(u))
        cor_hsic(x,y)
    }, error = function(err) {NA})
    return(cor_value)
}