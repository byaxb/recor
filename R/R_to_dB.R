#' Conversions among dB, SNR, R2, R
#'
#' @param R, R
#' @details
#' See https://arxiv.org/abs/2205.04571 for details
#' 
#' @return dB
#' @export
R_to_dB <- function(R) {
    R2_to_dB(R^2)
}
