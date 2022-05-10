#' bound of covariance - Rearrangement Covariance of x and y
#'
#' @description
#' Rearrangement Covariance of x and y
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' rearrangement covariance of x and y is defined as 
#' cov(sort(x, T), sort(y, T)) if cov(x, y) >= 0
#' cov(sort(x, T), sort(y, F)) if cov(x, y) < 0

#' 
#'
#' @return a numeric vector of length one
#' @export
cov_rearrangement <- function(x, y) {
    if(cov(x, y) >= 0) {
        return(cov(sort(x, decreasing = TRUE), sort(y, decreasing = TRUE)))
    } else {
        return(cov(sort(x, decreasing = FALSE), sort(y, decreasing = TRUE)))
    }
}
