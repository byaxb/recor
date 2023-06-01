#' Ball Correlation Statistics
#'
#' @description
#' A wrapper for ball() in Ball::bcor 
#' @usage
#' cor_ball(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help files for Ball::bcor 
#'
#' @return correlation coefficients
#' @import Ball
#' @export
cor_bcor <- function(x, y) {
    cor_value <- tryCatch(expr = {
        drop(bcor(x, y))
    }, error = function(err) {NA})
    return(cor_value)
}