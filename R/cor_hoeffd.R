#' Hoeffding's D Statistics
#'
#' @description
#' A wrapper for Hmisc::hoeffd()
#' @usage
#' cor_hoeffd(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for Hmisc::hoeffd
#'
#' @return correlation coefficients
#' @import Hmisc
#' @export
cor_hoeffd <- function(x, y) {
    cor_value <- tryCatch(expr = {
        drop(hoeffd(x, y)$D[1, 2])
    }, error = function(err) {NA})
    return(cor_value)
}
