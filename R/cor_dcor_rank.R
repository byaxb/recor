#' Distance Correlation Coefficient - rank version
#'
#' @description
#' A wrapper for energy::dcor
#' @usage
#' cor_dcor(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' 
#' calculate cor_dcor on rank(x) and rank(y)
#' 
#' Fast rank:
#' \itemize{
#'   \item data.table::frank
#'   \item pracma::Rank
#'   \item DescTools::Rank
#'   \item Rfast::Rank
#' }
#' 
#' Rfast::Rank is adpoted here since it is the fastest
#'
#' @return correlation coefficients
#' @import dcov
#' @import Rfast
#' @export
cor_dcor_rank <- function(x, y) {
    cor_value <- tryCatch(expr = {
        x <- Rank(x)
        y <- Rank(y)
        drop(sqrt(dcor2d(x, y)))
    }, error = function(err) {NA})
    return(cor_value)
    
}