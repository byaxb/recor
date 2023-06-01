#' Conversions among dB, SNR, R2, R
#'
#' @param R, R
#' @details
#' See https://arxiv.org/abs/2205.04571 for details
#' 
#' @return SNR
#' @export
R_to_SNR <- function(R) {
    R2_to_SNR(R^2)
}