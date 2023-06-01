#' Sharp version of cor_pearson
#'
#' @description
#' Sharp version of cor_pearson
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' the same as cor_tcor
#' 
#'
#' @return correlation coefficients
#' @export
recor_pearson <- function(x, y) {
    cor_tcor(x, y)
}
