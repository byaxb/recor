#' Alternating Conditional Expectations - Maximal Correlation
#'
#' @description
#' Uses the alternating conditional expectations algorithm to 
#' find the transformations of y and x that 
#' maximize the proportion of variation in y explained by x.
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for acepack::ace()
#'
#' @return correlation coefficients
#' @references 
#' L. Breiman, J. H. Friedman, 
#' Estimating Optimal Transformations for Multiple Regression and Correlation. 
#' Journal of the American Statistical Association. 80, 580–598 (1985)
#' @import acepack
#' @export
cor_ACE <- function(x, y) {
    cor_value <- tryCatch(expr = {
        sqrt(ace(x, y)$rsq)
    }, error = function(err) {NA})
    return(cor_value)
}
