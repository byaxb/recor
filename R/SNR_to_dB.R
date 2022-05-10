#' Transitions among SNR,SNRdB, r and R^2
#'
#' @description
#' transition between SNR and SNRdB 
#' 
#' @param SNR, signal-to-noise ratio, var(y_hat + noise) / var(noise)
#' @details
#' 
#' R2 <- SNR / (1 + SNR)
#' SNR <- R2 / (1 - R2)
#' 
#' SNRdB = 10*log10(SNR)
#' SNR = 10^(SNRdB/10)
#' 
#' 
#' @return corresponding SNRdB of SNR as a numeric vector
#' @export
SNR_to_dB <- function(SNR) {
    return(10*log10(SNR))
}


