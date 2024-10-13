#' Conversions among dB, SNR, R2, R
#'
#' @param SNR, SNR
#' @details
#' Details are described in the manuscript, 
#' which is available at https://arxiv.org/abs/2205.04571
#' 
#' 
#' @return dB
#' @export
SNR_to_dB <- function(SNR) {
    return(10*log10(SNR))
}


