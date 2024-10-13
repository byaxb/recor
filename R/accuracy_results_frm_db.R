#' Comprehensive results from several accuracy_db files
#'
#' @description
#' Extract and combine data.frames from several several accuracy_db files
#' @param accuracy_db_dir, the way to store the accuracy files
#' @param cor_types, correlations
#' @param scenario_type, type of scenarios
#' @details
#' 
#' \enumerate{
#'   \item Call accuracy_db() to generate the basic results
#'   \item Call accuracy_results_frm_db() to generate the accuracy results data.frame
#'   \item Call accuracy_plot_lite() to draw the figures
#' }
#' 
#' 
#' load_rbind.data.table() all possible accuracy_db() files into a whole
#' 
#' @return a data.frame
#' @export
accuracy_results_frm_db <- function(accuracy_db_dir = "accuracy_db",
                                    cor_types = c("loose_pearson", "cor_pearson", "sharp_pearson",
                                                  "cor_spearman", "cor_kendall", "cor_dhsic", 
                                                  "cor_dcor", "cor_mic", "cor_xicor"),
                                    scenario_type = c("accuracy_mon", "accuracy_notmon","accuracy", "Xks"),
                                    ...) {
    
    scenario_type <- match.arg(scenario_type)
    
    if(scenario_type == "Xks") {
        selected_scenarios <- scenarios2("Xks", with_expr = FALSE)
    } else if(scenario_type == "accuracy_mon"){
        selected_scenarios <- .accuracy_fun_monotonic
    } else if(scenario_type == "accuracy_notmon"){
        selected_scenarios <- .accuracy_fun_nonmonotonic
    } else {
        selected_scenarios <- c(.accuracy_fun_nonmonotonic, .accuracy_fun_nonmonotonic)
    }

    accuracy_results <- load_rbind.data.table(accuracy_db_dir)
    setDF(accuracy_results)
    accuracy_results <- accuracy_results %>%
        dplyr::filter(y_str %in% selected_scenarios &
                          cor_type %in% cor_types)
    return(accuracy_results)
}