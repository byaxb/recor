#' The degenerated r
#'
#' @description
#' The degenerated r
#' @param x, variable x
#' @param y, variable y
#' @details
#' See https://arxiv.org/abs/2205.04571 for more details.
#'
#' @return correlation coefficient
#' @export
r_arithmetic <- function(x, y) {
    cor_value <- tryCatch(expr = {
        cov(x, y) / bound_r_minus(x, y)
    }, error = function(err) {NA})
    return(cor_value)
}


#' @rdname r_arithmetic
#' @aliases r_prime
#' @export
r_prime <- r_arithmetic

#' @rdname r_arithmetic
#' @aliases r_prime
r_minus <- r_arithmetic