#' Transitions among SNR,SNRdB, r and R^2
#'
#' @description
#' transition between SNR and R^2
#' 
#' @param SNR, signal-to-noise ratio, a numeric vector
#' @details
#' 
#' R2 <- SNR / (1 + SNR)
#' SNR <- R2 / (1 - R2)
#' 
#' @return a vector of R2 values
#' @export
SNR_to_R2 <- function(SNR) {
    R2 <- SNR / (1 + SNR)
    return(R2)
}