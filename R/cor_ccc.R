#' Concordance correlation coefficient
#'
#' @description
#' A wrapper of yardstick::ccc_vec(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' 
#' See help file for yardstick::ccc_vec(x, y)
#' 
#' 
#' The following three ways can be employed to calculate ccc:  
#' 
#' 
#' \enumerate{
#'   \item DescTools::CCC(x, y)$rho.c$est
#'   \item epiR::epi.ccc(x, y)$rho.c$est
#'   \item yardstick::ccc_vec(x, y)
#' }
#'  
#' 
#' yardstick::ccc_vec(x, y) is the fastest, but its results are 
#' a bit different from the former two.
#' 
#'
#' @return correlation coefficients
#' @references 
#' Lin, L. (1989). A concordance correlation coefficient 
#' to evaluate reproducibility. Biometrics, 45 (1), 255-268.
#' @import yardstick
#' @export
cor_ccc <- function(x, y) {
    cor_value <- tryCatch(expr = {
        ccc_vec(x, y)
    }, error = function(err) {NA})
    return(cor_value)
}
