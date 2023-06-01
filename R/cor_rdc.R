#' The randomized dependence coefficient
#'
#' @description
#' Code from the paper:
#' The randomized dependence coefficient
#' @usage
#' cor_rdc(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' For more details, see D. Lopez-Paz, P. Hennig, B. Schölkopf, The randomized dependence coefficient, 
#' in: Advances in Neural Information Processing Systems, 2013: pp. 1–9.
#'
#' @return correlation coefficients
#' @export
cor_rdc <- function(x, y, k = 20, s = 1 / 6, f = sin) {
    cor_value <- tryCatch(expr = {
        x <- cbind(apply(as.matrix(x), 2, function(u)
            rank(u) / length(u)), 1)
        y <- cbind(apply(as.matrix(y), 2, function(u)
            rank(u) / length(u)), 1)
        x <- s / ncol(x) * x %*% matrix(rnorm(ncol(x) * k), ncol(x))
        y <- s / ncol(y) * y %*% matrix(rnorm(ncol(y) * k), ncol(y))
        drop(cancor(cbind(f(x), 1), cbind(f(y), 1))$cor[1])
    }, error = function(err) {NA})
    return(cor_value)
}


