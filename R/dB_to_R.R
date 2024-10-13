#' Conversions among dB, SNR, R2, R
#'
#' @param dB, dB
#' @details
#' Details are described in the manuscript, 
#' which is available at https://arxiv.org/abs/2205.04571
#' 
#' 
#' @return R
#' @export
dB_to_R <- function(dB) {
    (1+10^(-.1*dB))^-.5
}