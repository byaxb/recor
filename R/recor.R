#' Rearrangement correlation
#'
#' @description
#' to adjust the underestimation for monotonic dependence 
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' This function is able to sharpen arbitrary correlation 
#' which is sensitive to monotonicity,
#' providing a simple and unified interface.
#' 
#' However, it is at the cost of efficiency.
#' 
#' For example, recor dcor will takes twice the time 
#' as dcov2d(x, y) / dcov2d(x_up, y_updown).
#'
#' @return correlation coefficients
#' @import coop
#' @import Rfast
#' @export
recor <- function(x, y, method = "cor_pearson") {
    cor_value <- tryCatch(expr = {
        numerator <- do.call(method, list(x = x, y = y))
        if(covar(x, y) >= 0) {
            denominator <- do.call(method,
                                   list(x = Sort(x, descending = FALSE),
                                        y = Sort(y, descending = FALSE)))
        } else {
            denominator <- do.call(method,
                                   list(x = Sort(x, descending = FALSE),
                                        y = Sort(y, descending = TRUE)))
        }
        numerator / abs(denominator)
    }, error = function(err) {NA})
    return(cor_value)
}
