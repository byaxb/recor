#' Raincloud plots for power analysis
#'
#' @description
#' Raincloud plots, for comparison among different correlation statistics
#' @param power_results, the result of cal_power
#' @param power_results_path, if power_results is null, latest data will be loaded automatically from this directory
#' @param lower_dB, the lower bound of selected dB range, with -5 as default
#' @param upper_dB, the upper bound of selected dB range, with 5 as default
#' @param cor_types, selected correlation statistics to be visualized
#' @param scenario_type, which type of scenarios to be plotted
#' @param omit_indistinguished_scenarios, whether indistinguished scenarios are omitted, default TRUE
#' @param cor_colors, colors to distinguished whether correlation is sharp, with .six_favor_colors[c(1, 3)] as default
#' @param print, whether to print the plot
#' @param show_exprs, to show the expressions instead of cor_type strings, with TRUE as default
#' @param n_col, number of pannel columns, with 3 as default
#' @param cor_type_expr, cor_types and their corresponding expressions
#' @param dir, direction, with "v" as default
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
#' Make sure either power_results or power_results_path should be set correctly.
#' 
#' 
#' @return a ggplot object
#' @export
power_plot_lite <- function(power_results = NULL,
                            power_results_path = NULL,
                            lower_dB = .lower_dB, 
                            upper_dB = .upper_dB,
                            cor_types = NULL,
                            scenario_type = c("power_distinguish",
                                              "all",
                                              "accuracy", "accuracy_mon", "accuracy_notmon",
                                              "power", "power_mon", "power_notmon", 
                                              "extreme_mon_pairs"),
                            omit_indistinguished_scenarios = FALSE,
                            cor_colors = NULL,
                            print = TRUE,
                            show_exprs = TRUE,
                            n_col = 3,
                            cor_type_expr = supported_cor_expr(),
                            dir = "v",
                            ...) {
    if(is.null(power_results)) {
        loaded_obj <- load_latest(power_results_path)
        power_results <- get(loaded_obj, .GlobalEnv)
    }
    power_results[, by = .(cor_type, y_str, SNR), 
                  .(power = median(power, na.rm = TRUE))] %>%
        .[, dB := SNR_to_dB(as.numeric(SNR))] %>%
        .[dB >= .lower_dB & dB <= .upper_dB] -> power_results
    
    on_y_str <- unique(power_results$y_str)
    cat(sprintf("\n%d scenarios available:",
                length(on_y_str)), "\n", fill = TRUE)
    print(on_y_str, quote = FALSE)
    
    
    scenario_type <- match.arg(scenario_type)
    selected_scenario <- scenarios(type = scenario_type, with_expr = FALSE)
    
    power_results %>%
        .[y_str %in% selected_scenario ] -> power_results
    
    scenarios_subset <- intersect(unique(power_results$y_str), on_y_str) 
    cat(sprintf("\n%d scenarios considered by setting scenario_type = %s:",
                length(scenarios_subset),
                scenario_type),"\n", fill = TRUE)
    print(scenarios_subset, quote = FALSE)
    
    
    if(omit_indistinguished_scenarios) {
        power_results %>%
            .[ , by = .(cor_type, y_str), 
               .(power_mean = median(power, na.rm = TRUE))] %>%
            .[power_mean == 1] %>%
            .[ , y_str] %>%
            unique() -> indistinguished
        power_results %>%
            .[!(y_str %in% indistinguished)] -> power_results
        cat(sprintf("\n%d indistinguished scenarios are omitted:",
                    length(indistinguished)), "\n", fill = TRUE)
        print(indistinguished, quote = FALSE)
    }
    
    
    
    available_cor_types <- unique(power_results$cor_type)
    cat(sprintf("\n%d cor_types available:",
                length(available_cor_types)), "\n", fill = TRUE)
    print(available_cor_types, quote = FALSE)
    
    final_cor_types <- available_cor_types
    
    if(!is.null(cor_types)) {
        power_results %>%
            .[cor_type %in% cor_types]-> power_results
        final_cor_types <- intersect(cor_types, available_cor_types)
        cat(sprintf("\n%d cor_types are selected by setting cor_types = %s:",
                    length(final_cor_types),
                    paste(cor_types, collapse = ", ")), "\n", fill = TRUE)
        print(final_cor_types, quote = FALSE)
        
    }
    
    if(is.null(cor_colors) || length(cor_colors) < length(final_cor_types)) {
        cor_colors <- science_colours_interpolation(length(final_cor_types))
    } else {
        cor_colors <- cor_colors[1:length(final_cor_types)]
    }
    
    cat("\nMore details about the final ")
    print(describe(power_results))
    
    
    power_results[, 
           by = .(cor_type),
           .(median_power = median(power, na.rm = TRUE))] -> median_power_annotion_df
    
    df_plt <- as.data.frame(power_results)
    median_power_annotion_df <- as.data.frame(median_power_annotion_df)
    
    if(show_exprs) {
        
        type_levels <- cor_type_expr$cor_type
        type_labels <- cor_type_expr$cor_expr
        
        idx <- index_of_A_in_B(final_cor_types, type_levels)
        type_levels <- type_levels[idx]
        type_labels <- type_labels[idx]
        
        type_it <- function(x) {
            x <- as.character(x)
            factor(x, levels = type_levels, labels = type_labels)
        }
        
        df_plt %>%
            mutate(cor_type = type_it(cor_type)) %>%
            droplevels() %>%
            na.omit() -> df_plt
        
        median_power_annotion_df %>%
            mutate(cor_type = type_it(cor_type)) %>%
            droplevels() %>%
            na.omit() -> median_power_annotion_df
    }
    
    df_plt %>%
        ggplot(mapping =aes(x = 0, 
                            y = power, 
                            colour = cor_type,
                            group = NA)) +
        geom_half_boxplot(aes(fill = cor_type),
                          width = 1,
                          outlier.shape = NA)+
        geom_point(aes(x = -2, 
                       y = power, 
                       colour = cor_type, 
                       fill = cor_type),
                   position = position_jitter(width = 0.5),
                   size = 0.9,
                   alpha = 0.2) +
        geom_density(aes(x = after_stat(density),  
                         y = power, 
                         fill = cor_type),
                     color = NA) +
        geom_text(data = median_power_annotion_df,
                  aes(x = 0.5,
                      y = median_power,
                      label = sprintf("%1.3f", median_power)),
                  colour = "black",
                  family = .base_family)+
        scale_color_manual(values = alpha(cor_colors, 1)) +
        scale_fill_manual(values = alpha(cor_colors, 0.45)) +
        facet_wrap(~cor_type, 
                   ncol = n_col,
                   labeller = label_parsed,
                   scales = "free",
                   dir = dir) +
        coord_flip() +
        theme(legend.position = 'none') +
        labs(x = NULL,
             y = "Power") -> ggobj
    
    (ggplot_build(ggobj)$layout$panel_params[[1]]$y$get_labels() -> axis_y_labels)
    gg_com <- ggobj + scale_x_continuous(breaks = c(0, 2))
    if(print) {
        print(gg_com)
    }
    invisible(gg_com)
}
