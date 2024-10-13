#' Conversions among dB, SNR, R2, R
#'
#' @param R2, R2
#' @details
#' Details are described in the manuscript, 
#' which is available at https://arxiv.org/abs/2205.04571
#' 
#' 
#' @return dB
#' @export
R2_to_dB <- function(R2) {
    SNR <- R2_to_SNR(R2)
    return(SNR_to_dB(SNR))
}