#' Conversions among dB, SNR, R2, R
#'
#' @param dB, dB
#' @details
#' Details are described in the manuscript, 
#' which is available at https://arxiv.org/abs/2205.04571
#' 
#' 
#' @return R2
#' @export
dB_to_R2 <- function(dB) {
    SNR <- 10^(dB/10)
    return(SNR_to_R2(SNR))
}