#' Get the power_df
#'
#' @description
#' Calculate the power data.frame on a given confidence level
#' @usage
#' get_power_df(results, 0.95)
#' @param results, a list including null_resluts and alternative_results
#' @param alpha, confidence level, with 0.95 as default
#' @details
#' results is usually resulted by power_analysis()
#' recommended alpha values are 0.90, 0.95, and 0.99
#' 
#' @return power data.frame
#' @export
get_power_df <- function(results, alpha = 0.95) {

    get_cut_point <- function(results,
                              cur_sample_size,
                              cur_cor_fun,
                              cur_y_fun,
                              cur_noise_level,
                              alpha = 0.95) {
        targeted_results <- results$null_results[[cur_sample_size]][[cur_cor_fun]][[cur_y_fun]][, cur_noise_level]
        quantile(abs(targeted_results), alpha, na.rm = TRUE)
    }
    
    cal_power <- function(results,
                          cur_sample_size,
                          cur_cor_fun,
                          cur_y_fun,
                          cur_noise_level,
                          cur_cut_point) {
        targeted_results <- results$alternative_results[[cur_sample_size]][[cur_cor_fun]][[cur_y_fun]][, cur_noise_level]
        sum(abs(targeted_results) >= cur_cut_point) / length(targeted_results)
        
    }
    
    cor_fun_strs <- attr(results, 'cor_fun_strs')
    y_fun_strs <- attr(results, 'y_fun_strs')
    noise_levels <- attr(results, 'noise_levels')
    sample_sizes <- attr(results, 'sample_sizes')
    power_df <- NULL
    len_L1 <- length(results[[1]])
    len_L2 <- length(results[[1]][[1]])
    len_L3 <- length(results[[1]][[1]][[1]])
    len_L4 <- ncol(results[[1]][[1]][[1]][[1]])
    for(cur_sample_size in 1:len_L1) {
        for(cur_cor_fun in 1:len_L2) {
            for(cur_y_fun in 1:len_L3) {
                for(cur_noise_level in 1:len_L4) {
                    cur_cut_point <- get_cut_point(results,
                                                   cur_sample_size,
                                                   cur_cor_fun,
                                                   cur_y_fun,
                                                   cur_noise_level,
                                                   alpha)
                    cur_power <- cal_power(results,
                                           cur_sample_size,
                                           cur_cor_fun,
                                           cur_y_fun,
                                           cur_noise_level,
                                           cur_cut_point)
                    cur_power_row <- c(cor_fun = cor_fun_strs[cur_cor_fun],
                                       sample_size = sample_sizes[cur_sample_size],
                                       y_fun = y_fun_strs[cur_y_fun],
                                       noise_level = noise_levels[cur_noise_level],
                                       power = cur_power)
                    power_df <- rbind(power_df, cur_power_row)
                }
            }
        }
    }
    power_df %>%
        as.data.frame() %>%
        mutate(power = as.numeric(power)) %>% return()
}