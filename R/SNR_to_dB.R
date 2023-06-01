#' Conversions among dB, SNR, R2, R
#'
#' @param SNR, SNR
#' @details
#' See https://arxiv.org/abs/2205.04571 for details
#' 
#' @return dB
#' @export
SNR_to_dB <- function(SNR) {
    return(10*log10(SNR))
}


