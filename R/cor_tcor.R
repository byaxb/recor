#' Correlation with tighter bounds
#'
#' @description
#' Faster
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' 
#' cor_tcor is the rearrangement correlation.
#' 
#' t stands for tighter bound, which leads to sharp values
#'
#' @return correlation coefficients
#' @import coop
#' @import Rfast
#' @export
cor_tcor <- function(x, y) {
    cor_value <- tryCatch(expr = {
        numerator <- covar(x, y)
        if(numerator >= 0) {
            denominator <- abs(covar(Sort(x, descending = FALSE), 
                                     Sort(y, descending = FALSE)))
        } else {
            denominator <- abs(covar(Sort(x, descending = FALSE), 
                                     Sort(y, descending = TRUE)))
        }
        numerator / denominator
    }, error = function(err) {NA})
    return(cor_value)
}