#' calculate R2 with certain y_hat and y
#'
#' @description
#' 
#' @usage
#' cal_R2_SNR()
#' @param y, noised signal
#' @param y_hat, signal
#' @param noise, noise
#' @details
#' The following relationship is assumed: 
#' 
#' y <- y_hat + noise
#' 
#' That is, noise is independent sample added to signal y_hat.
#' 
#' Any two of the three arguments, y, y_hat, noise should be set.
#' 
#' There are different definitions of $\eqn{R^2}
#' For more details, see T.O. Kvålseth, Cautionary Note about \eqn{R^2}, The American Statistician. 39 (1985) 279–285. https://doi.org/10.1080/00031305.1985.10479448. 
#' 
#' @return power, scalar value
#' @export
cal_R2_SNR <- function(y = NULL, y_hat = NULL, noise = NULL) {
    if(is.null(y)) {
        y <- y_hat + noise
    }
    if(is.null(y_hat)) {
        y_hat <- y - noise
    }
    if(is.null(noise)) {
        noise <- y - y_hat
    }
    R2 <- 1 - var(noise) / var(y)
    SNR <- var(y_hat) / var(noise)
    return(list(R2 = R2,
                SNR = SNR,
                SNRdB = SNR_to_dB(SNR)))
}
