#' Accuracy analysis for different correlation statistics
#'
#' @description
#' Calculate the correlation coefficients for different types of functions
#' under different levels of noises 
#' to evaluate whether the correlation values are similar under certain noise
#' @param power_db_dir, the path of power_db, including several power_simulation_cor_type_fun1~107_timestamp.rda files
#' @param cor_types, correlation types, with c('loose_pearson', 'cor_pearson', 'sharp_pearson', 'cor_spearman',  'cor_kendall', 'cor_dhsic', 'cor_dcor', 'cor_mic', 'cor_xicor') as default 
#' @details
#' 
#' 
#' The workflow for complete power analysis is as follows: 
#' \enumerate{
#'   \item Call power_db() to generate the basic results
#'   \item Call power_results_frm_db() to generate the power results data.frame
#'   \item Call power_plot_lite() to draw the figures
#' }
#' 
#' See more details at https://arxiv.org/abs/2205.04571
#' 
#' @return power_results
#' @export
power_results_frm_db <- function(power_db_dir = "./power_db",
                                 cor_types = c('loose_pearson', 'cor_pearson', 'sharp_pearson',
                                               'cor_spearman', 'cor_kendall', 'cor_dhsic',
                                               'cor_dcor', 'cor_mic', 'cor_xicor'),
                                 ...) {

    power_results_lst <- NULL
    for(cor_type in cor_types) {
        power_df <- load_rbind.data.table(power_db_dir, 
                                          pattern = paste0("power_simulation_",
                                                           cor_type,
                                                           "_fun"))
        power_results_lst[[cor_type]] <- cal_power(power_df)
    }
    rbindlist(power_results_lst)
}
