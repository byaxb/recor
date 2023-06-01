#' Maximal Information Coefficient
#'
#' @description
#' A wrapper for minerva::mine_stat(x, y, measure = 'tic')
#' @usage
#' cor_tic(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for minerva::mine_stat
#'
#' @return correlation coefficients
#' @export
cor_tic <- function(x, y) {
    cor_value <- tryCatch(expr = {
        mine_stat(x, y, measure = 'tic', norm = TRUE)
    }, error = function(err) {NA})
    return(cor_value)
}
