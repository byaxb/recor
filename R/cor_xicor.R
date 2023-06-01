#' Compute the cross rank increment correlation coefficient xi
#'
#' @description
#' code from XICOR::calculateXI()
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for XICOR::calculateXI
#' 
#' 
#' https://github.com/zhexiaolin/Limit-theorems-of-Chatterjee-s-rank-correlation
#' 
#' @return correlation coefficients
#' @import XICOR
#' @export
cor_xicor <- function(x, y) {
    cor_value <- tryCatch(expr = {
        n <- length(x)
        PI <- rank(x, ties.method = "random")
        fr <- rank(y, ties.method = "max")/n
        gr <- rank((-y), ties.method = "max")/n
        ord <- order(PI)
        fr <- fr[ord]
        A1 <- sum(abs(fr[1:(n - 1)] - fr[2:n]))/(2 * n)
        CU <- mean(gr * (1 - gr))
        xi <- 1 - A1/CU
        xi
    }, error = function(err) {NA})
    return(cor_value)
}
