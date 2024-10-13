#' Conversions among dB, SNR, R2, R
#'
#' @param SNR, SNR
#' @details
#' Details are described in the manuscript, 
#' which is available at https://arxiv.org/abs/2205.04571
#' 
#' 
#' @return R
#' @export
SNR_to_R <- function(SNR) {
    sqrt(SNR_to_R2(SNR))
}