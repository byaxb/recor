#' Conversions among dB, SNR, R2, R
#'
#' @param R, R
#' @details
#' Details are described in the manuscript, 
#' which is available at https://arxiv.org/abs/2205.04571
#' 
#' 
#' @return dB
#' @export
R_to_dB <- function(R) {
    R2_to_dB(R^2)
}
