#' Transitions among SNR,SNRdB, r and R^2
#'
#' @description
#' transition between SNRdB and SNR
#' 
#' @param dB, Signal-to-Noise Ration in decibels
#' @details
#' dB = 10*log10(SNR)
#' SNR = 10^(db/10)
#' 
#' 
#' @return corresponding SNR of SNRdB
#' @export
dB_to_SNR <- function(dB) {
    SNR <- 10^(dB/10)
    return(SNR)
}