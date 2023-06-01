#' The Generalized Mean Information Coefficient
#'
#' @description
#' A wrapper for minerva::mine_stat(x, y, measure = 'gmic')
#' @usage
#' cor_gmic(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for minerva::mine_stat
#'
#' @return correlation coefficients
#' @references 
#' D. Albanese, M. Filosi, R. Visintainer, S. Riccadonna, G. Jurman, C. Furlanello. 
#' minerva and minepy: a C engine for the MINE suite and its R, Python and MATLAB wrappers. 
#' Bioinformatics (2013) 29(3): 407-408
#' @import minerva
#' @export
cor_gmic <- function(x, y) {
    cor_value <- tryCatch(expr = {
        drop(mine_stat(x, y, measure = 'gmic'))
    }, error = function(err) {NA})
    return(cor_value)
    
}
