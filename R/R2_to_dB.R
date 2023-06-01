#' Conversions among dB, SNR, R2, R
#'
#' @param R2, R2
#' @details
#' See https://arxiv.org/abs/2205.04571 for details
#' 
#' @return dB
#' @export
R2_to_dB <- function(R2) {
    SNR <- R2_to_SNR(R2)
    return(SNR_to_dB(SNR))
}