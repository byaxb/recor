#' Sharp version of distance correlation - dCor
#'
#' @description
#' sharp version of energy::dcor 
#' 
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#'  The sharp dCor is defined as 
#'  dCov(x, y) / abs(dCov_rearrangement(x, y)). 
#'  Here, the dCov_rearrangement(x, y) is similar to cov_rearranement(x, y)
#' 
#'
#' @return a numeric vector of length one
#' @export
sharp_dcor <- function(x, y, index = 1) {
    if(cov(x, y) >= 0) {
        return(dcov(x, y, index) /
                   dcov(sort(x, decreasing = FALSE), 
                        sort(y, decreasing = FALSE), index))
    } else {
        return(dcov(x, y, index) /
                   dcov(sort(x, decreasing = FALSE), 
                        sort(y, decreasing = TRUE), index))
    }
}