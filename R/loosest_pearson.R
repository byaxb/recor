#' Even looser version of Pearson's r, identical without shifts
#'
#' @description
#' loose version of Pearson's r
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' Scale covariance with an even looser bound than that 
#' by Cauchy-Schwarz Inequality
#'
#' @return Coefficient of identity
#' @import coop
#' @export
loosest_pearson <- function(x, y) {
    cor_value <- tryCatch(expr = {
        2*covar(x, y) / (covar(x, x) + covar(y, y) + (mean(x) - mean(y))^2)
    }, error = function(err) {NA})
    return(cor_value)
}