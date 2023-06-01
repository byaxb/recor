#' Conversions among dB, SNR, R2, R
#'
#' @param R2, R2
#' @details
#' See https://arxiv.org/abs/2205.04571 for details
#' 
#' @return SNR
#' @export
R2_to_SNR <- function(R2) {
    R2 / (1 - R2)
}