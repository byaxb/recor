#' To get the theoretical r scores regarding to different SNR / dB
#'
#' @description
#' To get the theoretical r scores regarding to different SNR / dB
#' 
#' @param dB, signal-to-noise ratio in decibels, with -50:50 as default
#' @details
#' r = sqrt(1 / (1 + 10^(-0.1dB)))
#' @return power, scalar value
#' @export
r_score_theoretical <- function(dB = -50:50) {
    data.frame(dB = dB,
               cor_coeff = sqrt(10^(0.1*dB)/(1+10^(0.1*dB))))
}
