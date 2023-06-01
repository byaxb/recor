#' Percentage bend correlation
#'
#' @description
#' A wrapper for asbio::r.pb
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' 
#' See help file for asbio::r.pb
#' 
#' The results is the same as WRS2::pbcor, 
#' but asbio::r.pb is faster.
#'
#' @return correlation coefficients
#' @import asbio
#' @references 
#' R. R. Wilcox, Introduction to Robust Estimation and Hypothesis Testing, 3rd edtion. 
#' Academic Press, 2012. pp.446
#' @export
cor_percentage_bend <- function(x, y, beta = 0.2) {
    cor_value <- tryCatch(expr = {
        r.pb(x, y, beta = beta)$r.bp
    }, error = function(err) {NA})
    return(cor_value)
}
