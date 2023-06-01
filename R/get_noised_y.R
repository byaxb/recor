#' Get noised y
#'
#' @description
#' Get noised y with certain SNR
#' @param y_hat, signal
#' @param SNR, signal-to-noise rate
#' @details 
#' The noised y is calculated in the following way:
#' y = y_hat + noise
#' 
#' Either SNR or dB should be set to control the noise level.
#' 
#' @return power, scalar value
#' @export
get_noised_y <- function(y_hat, SNR = 1, dB = NULL) {
    if(!is.null(dB)) {
        SNR <- dB_to_SNR(dB)
    }
    noise_to_be_added <- gen_noise(y_hat, SNR)
    y_noised <- y_hat + noise_to_be_added
    return(y_noised)
}


