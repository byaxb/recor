#' Generate the accuracy database
#'
#' @description
#' To generate the accuracy database and save it into a certain directory
#' @param accuracy_db_dir, the way to store the accuracy files
#' @param cor_types, which correlations will be calculated
#' @param scenario_type, which type of scenarios will be selected
#' @details
#' 
#' The workflow for complete accuracy analysis is as follows: 
#' \enumerate{
#'   \item Call accuracy_db() to generate the basic results
#'   \item Call accuracy_results_frm_db() to generate the accuracy results data.frame
#'   \item Call accuracy_plot_lite() to draw the figures
#' }
#' 
#' See more details at https://arxiv.org/abs/2205.04571
#' 
#' scenario_type = "accuracy", 50 monotonic functions along with 
#' 16 nonmonotonic scenarios are taken into consideration.
#' 
#' scenario_type = "Xks", y <- X^k with k = seq(1, 10, by = 0.1) are involved. 
#'  
#' Make sure a sub-folder named as "accuracy_db" under the current work directory 
#' is created before running this function. Another choice is to specify 
#' the accuracy_db_dir to an existing empty folder.
#'  
#' @return several accuracy_XXXXX_.rda files are generated at the specified directory
#' @export
accuracy_db <- function(accuracy_db_dir = "accuracy_db",
                        cor_types = c("loose_pearson", "cor_pearson", "sharp_pearson",
                                      "cor_spearman", "cor_kendall", "cor_dhsic", 
                                      "cor_dcor", "cor_mic", "cor_xicor"),
                        scenario_type = c("accuracy", "Xks"),
                        ...) {
    
    scenario_type <- match.arg(scenario_type)
    
    od <- getwd()
    setwd(accuracy_db_dir)
    timeit('new')
    for(cor_type in cor_types){
        if(scenario_type == "accuracy") {
            selected_y_funs <- c(.accuracy_fun_monotonic, .accuracy_fun_nonmonotonic)
            resulted_cor_df <- accuracy_analysis(y_strs = selected_y_funs, 
                                                 cor_strs = cor_type)
            save(resulted_cor_df,
                 file = add_time_ext(paste0("accuracy_fun66_",
                                            cor_type,
                                            ".rda")))
        } else if(scenario_type == "Xks") {
            selected_y_funs <- paste0("y <- x^", seq(1, 10, by = 0.1))
            resulted_cor_df <- accuracy_analysis(y_strs = selected_y_funs, 
                                                 cor_strs = cor_type)
            save(resulted_cor_df,
                 file = add_time_ext(paste0("accuracy_Xks_",
                                            cor_type,
                                            ".rda")))    
        } else {
            cat("Unknown scenario_type")
        }
    }
    setwd(od)
}