#' Get the theoretical r scores with resepct to different SNRdB
#'
#' @description
#' get the theoretical r scores with resepct to different SNRdB
#' 
#' @param dB, Signal-to-Noise Ration in decibels, with -50:50 as default
#' @details
#' r = sqrt(1 / (1 + 10^(-0.1dB)))
#' 
#' 
#' @return theoretical r scores in a numeric vector 
#' @export
r_score_theoretical <- function(dB = -50:50) {
    data.frame(dB = dB,
               cor_coeff = sqrt(10^(0.1*dB)/(1+10^(0.1*dB))))
}
