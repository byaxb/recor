#' Calculate the power
#'
#' @description
#' Power calculation on the basis of the results of power analysis
#' @param power_df, data.frame, the result of sim_power()
#' @param alpha, the power threshold, with 1-0.05 = 0.95 as default 
#' @details
#' The empirical power at type I error level alpha=0.05 is calculated as 
#' the fraction of dependent variable pairs yielding a statistic value 
#' greater than 95% of the values yielded by the independent variable pairs.
#' 
#' Here, alpha should be set as 0.95 rather than 0.05
#'
#' @return a data.frame
#' @export
cal_power <- function(power_df, alpha = 0.95) {
    if(alpha <=0 || alpha >= 1) {
        cat("Invalid alpha. Reset as 0.95")
        alpha <- 0.95
    }
    power_df <- as.data.table(power_df)
    return(power_df[,
                    by = .(round_idx, y_str,SNR, cor_type),
                    .(power = mean(abs(cor_alternative) >= quantile(abs(cor_null), alpha, na.rm = TRUE)))])
    
}