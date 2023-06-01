#' Conversions among dB, SNR, R2, R
#'
#' @param dB, dB
#' @details
#' See https://arxiv.org/abs/2205.04571 for details
#' 
#' @return SNR
#' @export
dB_to_SNR <- function(dB) {
    SNR <- 10^(dB/10)
    return(SNR)
}