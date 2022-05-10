#' Transitions among SNR,SNRdB, r and R^2
#'
#' @description
#' transition between R2 and SNRdB#' 
#' 
#' @param R2, Coefficient of determination R^2
#' @details
#' R2 <- SNR / (1 + SNR)
#' SNR <- R2 / (1 - R2)
#' 
#' SNRdB = 10*log10(SNR)
#' SNR = 10^(SNRdB/10)
#' 
#' 
#' @return corresponding SNRdB of the given R^2 as a numeric vector
#' @export
R2_to_SNR <- function(R2) {
    SNR <- R2 / (1 - R2)
    return(SNR)
}