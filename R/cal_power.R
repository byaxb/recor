#' Calculate the power of correlation statistics
#'
#' @description
#' Power calculation on the results of power analysis
#' @param power_analysis_df, data.frame, the result of sim_power()
#' @param alpha, the type I alpha, with 0.95 as default
#' @details
#' The empirical power at type I error level alpha=0.95 is calculated as 
#' the fraction of dependent variable pairs yielding a statistic value 
#' greater than 95% of the values yielded by the independent variable pairs.
#'
#' @return power data.frame
#' @export
cal_power <- function(power_analysis_df, alpha = 0.95) {
    if(alpha <=0 || alpha >= 1) {
        cat("Invalid alpha. Reset as 0.95")
        alpha <- 0.95
    }
    power_analysis_df %>%
        as.data.frame() %>%
        group_by(x_str, y_str, SNRdB, cor_type) %>%
        summarise(power = sum(cor_alternative >= 
                                  quantile(cor_null, 
                                           alpha, 
                                           na.rm = TRUE))/n()) -> power_df
    return(power_df)
}

