#' Sharp version of Ball Correlation - BCor
#'
#' @description
#' sharp version of Ball::bcor 
#' 
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#'  The sharp BCor is defined as 
#'  BCov(x, y) / abs(BCov_rearrangement(x, y)). 
#'  Here, the BCov_rearrangement(x, y) is similar to cov_rearranement(x, y)
#'
#' @return a numeric vector of length one
#' @export
sharp_bcor <- function(x, y) {
    if(cov(x, y) >= 0) {
        return(bcov(x, y) /
                   bcov(sort(x, decreasing = FALSE), 
                        sort(y, decreasing = FALSE)))
    } else {
        return(bcov(x, y) /
                   bcov(sort(x, decreasing = FALSE), 
                        sort(y, decreasing = TRUE)))
    }
}
