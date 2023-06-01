#' Biweight Midcorrelation
#'
#' @description
#' A wrapper for WGCNA::bicor
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help files for WGCNA::bicor
#'
#' @return correlation coefficients
#' @import WGCNA
#' @references 
#' R. R. Wilcox, Introduction to Robust Estimation and Hypothesis Testing, 
#' 3rd edtion. Academic Press, 2012. Chapter 9. pp.446
#' 
#' Peter Langfelder, Steve Horvath (2012) Fast R Functions for 
#' Robust Correlations and Hierarchical Clustering. 
#' Journal of Statistical Software, 46(11), 1-17.
#'  https://www.jstatsoft.org/v46/i11/
#' 
#' @export
cor_bicor <- function(x, y) {
    cor_value <- tryCatch(expr = {
        drop(bicor(x, y))
    }, error = function(err) {NA})
    return(cor_value)
}
