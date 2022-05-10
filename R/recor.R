#' REarrangement-type CORrelation - the adjusted r
#'
#' @description
#' correlation based on rearrangement covariance - the adjusted r
#' 
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' the adjusted r is defined as 
#' cov(x, y) / abs(cov_rearrangement(x, y))
#'
#' @return numeric vector of length one
#' @export
recor <- function(x, y) {
    numerator <- cov(x, y)
    if(numerator >= 0) {
        denominator <- abs(cov(sort(x, decreasing = FALSE), 
                               sort(y, decreasing = FALSE)))
    } else {
        denominator <- abs(cov(sort(x, decreasing = FALSE), 
                               sort(y, decreasing = TRUE)))
    }
    return(numerator / denominator)
}