#' Conversions among dB, SNR, R2, R
#'
#' @param dB, dB
#' @details
#' See https://arxiv.org/abs/2205.04571 for details
#' 
#' @return R2
#' @export
dB_to_R2 <- function(dB) {
    SNR <- 10^(dB/10)
    return(SNR_to_R2(SNR))
}