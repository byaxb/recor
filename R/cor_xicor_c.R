#' Estimate the conditional dependence coefficient (CODEC) xi_cor
#'
#' @description
#' A wrapper of FOCI::codec
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for FOCI::codec
#' 
#' The conditional dependence coefficient (CODEC) 
#' is a measure of the amount of conditional dependence 
#' between a random variable Y and a random vector Z 
#' given a random vector X, based on an i.i.d. sample of (Y, Z, X). 
#' The coefficient is asymptotically guaranteed to be between 0 and 1.
#' 
#' @return correlation coefficients
#' @references 
#' M. Azadkia, S. Chatterjee, A simple measure of conditional dependence. 
#' Ann. Statist. 49 (2021), doi:10.1214/21-AOS2073
#' @import FOCI
#' @export
cor_xicor_c <- function(x, y) {
    cor_value <- tryCatch(expr = {
        codec(x, y)
    }, error = function(err) {NA})
    return(cor_value)
}
