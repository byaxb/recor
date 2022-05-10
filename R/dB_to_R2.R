#' Transitions among SNR,SNRdB, r and R^2
#'
#' @description
#' transition between SNRdB and R^2
#'
#' @param dB, Signal-to-Noise Ration in decibels
#' @details
#' 
#' R2 <- SNR / (1 + SNR)
#' SNR <- R2 / (1 - R2)
#' 
#' SNRdB = 10*log10(SNR)
#' SNR = 10^(SNRdB/10)
#' 
#' 
#' @return corresponding R^2 score of SNRdB as a numeric vector
#' @export
dB_to_R2 <- function(dB) {
    SNR <- 10^(dB/10)
    return(SNR_to_R2(SNR))
}