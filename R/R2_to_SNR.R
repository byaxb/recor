#' Transitions among SNR,SNRdB, r and R^2
#'
#' @description
#' transition between R2 and SNR#' 
#' 
#' @param R2, Coefficient of determination R^2
#' @details
#' R2 <- SNR / (1 + SNR)
#' SNR <- R2 / (1 - R2)
#' 
#' @return corresponding SNR of the given R^2 as a numeric vector
#' @export
R2_to_dB <- function(R2) {
    SNR <- R2 / (1 - R2)
    return(SNR_to_dB(SNR))
}