#' X1 - X2 for general correlation statistics
#' 
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' X1 - X2 is the same as 
#' that in a general correlation statistics 
#' proposed by Kendall#' 
#' @return numeric vector
#' @export
x1_minus_x2 <- function(x) {
    nx <- length(x)
    minus_results <- NULL
    for(i in 1:nx) {
        for(j in 1:nx) {
            minus_results <- c(minus_results,
                               x[j] - x[i])
        }
    }
    return(minus_results)
}
