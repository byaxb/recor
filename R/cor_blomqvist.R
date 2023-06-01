#' Blomqvist's beta
#'
#' @description
#' A wrapper for wdm::wdm(x, y = NULL, method = "blomqvist", weights = NULL, remove_missing = TRUE)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help files for wdm::wdm
#'
#' @return correlation coefficients
#' @import wdm
#' @references 
#' N. Blomqvist, On a measure of dependence between two random variables. 
#' The Annals of Mathematical Statistics, 593–600 (1950).
#' @export
cor_blomqvist <- function(x, y) {
    cor_value <- tryCatch(expr = {
        wdm(x, y, method = "blomqvist")
    }, error = function(err) {NA})
    return(cor_value)
}
