#' A warapper for 1_cal_power.R
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
#' @return a data.frame containing the correlatin coefficient values of different types in different scenarios
#' @import foreach
#' @import doSNOW
#' @export
power_db <- function(power_db_dir = "./power_db",
                     cor_types = c('loose_pearson', 'cor_pearson', 'sharp_pearson',
                                   'cor_spearman', 'cor_kendall', 'cor_dhsic',
                                   'cor_dcor', 'cor_mic', 'cor_xicor'),
                     ...) {
    old_wd <- getwd()
    setwd(power_db_dir)
    power_scenarios <- scenarios("power", with_expr = FALSE)
    for(cor_type in cor_types) {
        for(i in seq_along(power_scenarios)) {
            cur_y_fun <- power_scenarios[i]
            results <- sim_power(y_strs = cur_y_fun, 
                                 cor_strs = cor_type)
            current_pafilename <- paste0("power_simulation_",
                                         cor_type,
                                         "_fun", i,
                                         ".rda")
            save(results,
                 file = add_time_ext(current_pafilename))
            timeit(current_pafilename)
            
        }
        
    }
    setwd(old_wd)
}
