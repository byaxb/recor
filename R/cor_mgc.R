#' Multiscale Graph Correlation
#'
#' @description
#' A wrapper for mgc::mgc.stat
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' 
#' See help file for mgc::mgc.stat
#' 
#' @return sample distance correlation
#' @import mgc
#' @references 
#' J. T. Vogelstein, E. W. Bridgeford, Q. Wang, C. E. Priebe, M. Maggioni, C. Shen, 
#' Discovering and deciphering relationships across disparate data modalities. eLife. 8, e41690 (2019)
#' @export
cor_mgc <- function(x, y) {
    cor_value <- tryCatch(expr = {
        nx <- length(x)
        matrix_x <- matrix(x, nrow = nx)
        matrix_y <- matrix(y, nrow = nx)
        mgc.stat(matrix_x, matrix_y, is.dist.X = FALSE, is.dist.Y = FALSE)$stat
    }, error = function(err) {NA})
    return(cor_value)
    
}