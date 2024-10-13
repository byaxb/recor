#' Conversions among dB, SNR, R2, R
#'
#' @param R2, R2
#' @details
#' Details are described in the manuscript, 
#' which is available at https://arxiv.org/abs/2205.04571
#' 
#' 
#' @return SNR
#' @export
R2_to_SNR <- function(R2) {
    R2 / (1 - R2)
}