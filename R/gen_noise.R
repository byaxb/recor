#' Generate noise with certain SNR
#'
#' @description
#' generate noise for certain signal on certain SNR
#' @usage
#' gen_noise(y_hat, SNR = 2)
#' @param y_hat, pure signal, or the value of y_hat = f(x)
#' @param SNR, signal-to-noise ratio, with dB_to_SNR(0) as default
#' @details
#' SNR is defined as follows:
#' SNR = var(y_hat) / var(noise)
#' 
#' signal: y_hat, f(x)
#' noise: usually gaussian noise, noise~k*N(0, 1)
#' 
#' SNR should always be positive
#' 
#' if SNR <= 0, it will be replaced by .Machine$double.eps
#' 
#' if SNR is NA, it will be replaced by Inf
#' 
#' when SNR is Inf, noise will be zero-vectors, that is, no noise 
#' 
#' SNR can be calculated with dB_to_SNR:
#' 
#' For example, dB_to_SNR(20) = 100 which means \eqn{SNR=var(signal)/var(noise)=100} <=> 20dB
#' 
#' For another example, dB_to_SNR(0) = 1 which means \eqn{var(signal) == var(noise)}
#' 
#' @return noise, a numeric vector
#' @export
gen_noise <- function(y_hat, SNR = dB_to_SNR(0)) {
    noise <- rnorm(length(y_hat))
    noise <- noise - mean(noise)
    if(is.na(SNR)) {
        SNR <- Inf
    }
    if(SNR <= 0) {
        SNR <- .Machine$double.eps
    } 
    k <- sqrt(var(y_hat)/SNR)
    noise <- k*noise
    # attr(noise, 'k') <- k
    # attr(noise, 'SNR') <- SNR
    # attr(noise, 'y_hat') <- y_hat
    return(noise)
}


