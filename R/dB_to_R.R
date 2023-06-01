#' Conversions among dB, SNR, R2, R
#'
#' @param dB, dB
#' @details
#' See https://arxiv.org/abs/2205.04571 for details
#' 
#' @return R
#' @export
dB_to_R <- function(dB) {
    (1+10^(-.1*dB))^-.5
}