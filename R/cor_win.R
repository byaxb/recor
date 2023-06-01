#' Winsorized correlation
#'
#' @description
#' A wrapper for WRS2::wincor
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for WRS2::wincor
#' 
#'
#' @return correlation coefficients
#' @import WRS2
#' @export
cor_win <- function(x, y) {
    cor_value <- tryCatch(expr = {
        wincor(x, y)$cor
    }, error = function(err) {NA})
    return(cor_value)
}
