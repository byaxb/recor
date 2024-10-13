#' Conversions among dB, SNR, R2, R
#'
#' @param dB, dB
#' @details
#' Details are described in the manuscript, 
#' which is available at https://arxiv.org/abs/2205.04571
#' 
#' 
#' @return SNR
#' @export
dB_to_SNR <- function(dB) {
    SNR <- 10^(dB/10)
    return(SNR)
}