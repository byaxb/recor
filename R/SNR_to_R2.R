#' Conversions among dB, SNR, R2, R
#'
#' @param SNR, SNR
#' @details
#' See https://arxiv.org/abs/2205.04571 for details
#' 
#' @return R2
#' @export
SNR_to_R2 <- function(SNR) {
    SNR / (1 + SNR)
}