#' Maximal information coefficient
#'
#' @description
#' A wrapper for minerva::mine_stat(x, y, measure = 'mic')
#' @usage
#' cor_mic(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See the help file for minerva::mine_stat
#'
#' @return correlation coefficients
#' @export
cor_mic <- function(x, y) {
    cor_value <- tryCatch(expr = {
        mine_stat(x, y, measure = 'mic')
    }, error = function(err) {NA})
    return(cor_value)
    
}