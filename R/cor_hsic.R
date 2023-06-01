#' HSIC
#'
#' @description
#' HSIC
#' @usage
#' cor_hsic(x, y)
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' 
#' Attention: colculation on the same x and y, 
#' the resulted correlation coefficient will be different 
#' because of the random effect of kernlab::sigest
#' 
#' 
#' Codes from  
#' https://github.com/stochasticresearch/copulastatistic
#' 
#' which is further extracted from 
#' 
#' Code from Lopez-Paz 
#' https://github.com/lopezpaz/randomized_dependence_coefficient
#'
#' @return correlation coefficients
#' @import kernlab
#' @export
cor_hsic <- function(x, y) {
    
    computeKernelMatrix <- function(sample) {
        n        <- nrow(sample)
        Q        <- matrix(apply(sample^2, 1, sum), n, n)
        distance <- Q + t(Q) - 2 * sample %*% t(sample)
        exp(-sigest(sample,scale=NULL)[2]*distance)
    }
    
    hsic <- function(x, y) {
        N  <- nrow(as.matrix(x))
        K  <- computeKernelMatrix(as.matrix(x))
        L  <- computeKernelMatrix(as.matrix(y))
        KH <- K - 1 / N * matrix(apply(K, 2, sum), N, N)
        LH <- L - 1 / N * matrix(apply(L, 2, sum), N, N)
        1 / N * sum(sum(KH * t(LH)))
    }
    
    cor_value <- tryCatch(expr = {
        hsic(x, y)
    }, error = function(err) {NA})
    return(cor_value)
}