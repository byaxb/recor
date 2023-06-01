#' General Multidimensional Associations, or Generalized R-squared
#'
#' @description
#' A wrapper for matie::ma()
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for matie::ma
#'
#' @return correlation coefficients
#' @references 
#' Murrell, B., Murrell, D. & Murrell, H. 
#' Discovering General Multidimensional Associations. 
#' PLoS ONE 11, e0151551 (2016). Also available at: 
#' http://arxiv.org/abs/1303.1828
#' 
#' Q. Tan, M. Thomassen, M. Burton, K. F. Mose, K. E. Andersen, 
#' J. Hjelmborg, T. Kruse, Generalized Correlation Coefficient 
#' for Non-Parametric Analysis of Microarray Time-Course Data. 
#' Journal of Integrative Bioinformatics. 14 (2017), doi:10.1515/jib-2017-0011.
#' 
#' @import matie
#' @export
cor_Ahat <- function(x, y) {
    cor_value <- tryCatch(expr = {
        drop(ma(cbind(x, y))$A)
    }, error = function(err) {NA})
    return(cor_value)
}
