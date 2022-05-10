#' Generate noised y with certain SNR
#'
#' @description
#' generate noise for certain signal at certain SNR
#' 
#' @param y_hat, signal, y_hat = f(x)
#' @param SNR, Signal-to-Noise Ration
#' @param dB, Signal-to-Noise Ration in decibels
#' @details
#' noise signal = signal + noise
#'
#' y = y_hat + noise
#'
#'
#' @return noised y, i.e., f(x) + noise
#' @export
get_noised_y <- function(y_hat, SNR = 1, dB = NULL) {
    if(!is.null(dB)) {
        SNR <- dB_to_SNR(dB)
    }
    noise_to_be_added <- gen_noise(y_hat, SNR)
    y_noised <- y_hat + noise_to_be_added
    attr(y_noised, 'k') <- attr(noise_to_be_added, 'k')
    attr(y_noised, 'SNR') <- attr(noise_to_be_added, 'SNR')
    attr(y_noised, 'noise') <- noise_to_be_added
    return(y_noised)
}


