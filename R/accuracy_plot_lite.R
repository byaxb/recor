#' Accuracy Analysis Plot
#'
#' @description
#' plot the results of accuracy analysis with ggplot2
#' @param accuracy_results, accuracy results form accuracy_db()
#' @param cor_types, correlation names
#' @param anchor_colors, colors
#' @param supported_cor_types, different types of functions, such as x^k (several k)
#' @param supported_cor_exprs, noise levels, with dB_to_SNR(0:100) as default
#' @param mae_txt_format, the MAE text format
#' @param direction, the direction for facet_wrap, with "v" as default
#' @param n_row, number of rows for facet_wrap
#' @param n_row_legend, number of rows in the legend
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
#' 
#' 
#' @return a ggplot2 object
#' @export
accuracy_plot_lite <- function(accuracy_results,
                               cor_types = NULL,
                               anchor_colors = science_colours_six()[c(1,2)],
                               supported_cor_types = supported_cor_expr()$cor_type,
                               supported_cor_exprs = supported_cor_expr()$cor_expr,
                               mae_txt_format = "%1.3f",
                               direction = "v",
                               n_row = 3,
                               n_row_legend = 5,
                               ...) {
    
    accuracy_results %>%
        mutate(cor_coeff = abs(cor_coeff)) -> accuracy_results
    
    (n_y_str <- length(unique(accuracy_results$y_str)))

    anchor_colors <- science_colours_six()[c(1,2)]
    
    if(!is.null(cor_types)) {
        cor_types_idx <- index_of_A_in_B(cor_types, supported_cor_expr()$cor_type)
        supported_cor_types <- supported_cor_expr()$cor_type[cor_types_idx]
        supported_cor_exprs <- supported_cor_expr()$cor_expr[cor_types_idx]
    }
    
    if(all(unique(accuracy_results$y_str) %in% c(.accuracy_fun_monotonic, .accuracy_fun_nonmonotonic))) {
        scenario_type <- "accuracy"
    } else if(all(unique(accuracy_results$y_str) %in% scenarios2("Xks", with_expr = FALSE))) {
        scenario_type <- "Xks"
    } else {
        stop("Only purely either fun66 or Xks scenario type is supported. Please check the accuracy results")
    }
    accuracy_results %>%
        as.data.frame() %>%
        mutate(dB = SNR_to_dB(SNR),
               cor_coeff = abs(cor_coeff)) %>%
        group_by(cor_type, y_str, dB) %>%
        summarise(cor_coeff = median(cor_coeff, na.rm = TRUE)) -> cor_df_sum
    cor_df_sum %>%
        as.data.frame() %>%
        group_by(cor_type,  dB) %>%
        summarise(cor_coeff = median(cor_coeff, na.rm = TRUE)) %>%
        mutate(cor_type = factor(cor_type,
                                 levels = supported_cor_types,
                                 labels = supported_cor_exprs)) %>%
        droplevels() %>%
        na.omit() %>%
        mutate(R = dB_to_R(dB)) -> cor_df_sum_plt_df
    cor_df_sum %>%
        pivot_wider(names_from = dB,
                    values_from = cor_coeff) -> cor_df_sum_wider
    cor_df_sum_ref <- r_score_theoretical()
    r_analogy <- apply(cor_df_sum_wider[, -(1:2)], 1, function(cur_row) {
        r_sharp_scores <- as.numeric(cur_row)
        r_scores <- cor_df_sum_ref %>% pull(cor_coeff) %>% as.numeric()
        r_prime(abs(r_sharp_scores), abs(r_scores))
    })
    
    cbind(cor_df_sum_wider[, 1:2],
          analogy = r_analogy) %>%
        as.data.frame() -> cor_fun_r_analogy
    
    accuracy_results%>%
        mutate(R = SNR_to_R(SNR),
               cor_coeff = abs(cor_coeff)) %>%
        group_by(cor_type) %>%
        summarise(coeff_mae = mean(abs(cor_coeff - R), na.rm = TRUE)) %>%
        as.data.frame() %>%
        mutate(r_analogy_mean_txt = as.character(sprintf(mae_txt_format, coeff_mae))) %>%
        mutate(cor_type = factor(cor_type,
                                 levels = supported_cor_types,
                                 labels = supported_cor_exprs)) %>%
        droplevels() %>%
        na.omit() -> annotion_df
    
    if(scenario_type == "accuracy") {
        cor_fun_r_analogy %>%
            group_by(y_str) %>%
            summarise(analogy_median_y_str = mean(analogy, na.rm = TRUE)) %>%
            as.data.frame() %>%
            arrange(desc(analogy_median_y_str)) %>%
            pull(y_str) %>%
            index_of_A_in_B(y_funs_level_label()$level) -> selected_y_str_idx
        
        
        accuracy_results %>%
            as.data.frame() %>%
            mutate(dB = SNR_to_dB(SNR),
                   cor_coeff = abs(cor_coeff)) %>%
            group_by(cor_type, y_str, dB) %>%
            summarise(
                cor_coeff_Q3 = quantile(cor_coeff, 0.75, na.rm = TRUE),
                cor_coeff_Q1 = quantile(cor_coeff, 0.25, na.rm = TRUE),
                cor_coeff_median = mean(cor_coeff, na.rm = TRUE)) %>%
            pivot_longer(cols = c(
                cor_coeff_Q3,
                cor_coeff_Q1,
                cor_coeff_median),
                names_to = 'cor_coeff_type',
                values_to = 'cor_coeff') %>%
            mutate(y_str = factor(y_str,
                                  levels = y_funs_level_label()$level[selected_y_str_idx],
                                  labels = y_funs_level_label()$label[selected_y_str_idx]),
                   cor_type = factor(cor_type,
                                     levels = supported_cor_types,
                                     labels = supported_cor_exprs)) %>%
            droplevels() -> accuracy_results2plt
        
        cor_df_sum_ref %>%
            mutate(R = dB_to_R(dB)) -> cor_df_sum_ref
        
        accuracy_results %>%
            as.data.frame() %>%
            mutate(dB = SNR_to_dB(SNR),
                   cor_coeff = abs(cor_coeff)) %>%
            dplyr::filter(near(dB, .dB_0.25) |
                              near(dB, .dB_0.50) |
                              near(dB, .dB_0.75) |
                              near(dB, .dB_1.00)) %>%
            mutate(y_str = factor(y_str,
                                  levels = y_funs_level_label()$level[selected_y_str_idx],
                                  labels = y_funs_level_label()$label[selected_y_str_idx]),
                   cor_type = factor(cor_type,
                                     levels = supported_cor_types,
                                     labels = supported_cor_exprs)) %>%
            droplevels() %>%
            mutate(R = dB_to_R(dB)) -> accuracy_results_selected_dB
        
        
        pd <- position_dodge(0.1)
        
        accuracy_results2plt %>%
            mutate(R = dB_to_R(dB)) %>%
            ggplot(aes(x = R, y = cor_coeff,  color = y_str)) +
            geom_line(aes(group = paste0(cor_coeff_type, y_str)), linewidth = 1.5) +
            scale_colour_manual(values = alpha(science_colours_interpolation(n_y_str,anchor_colors), 0.15),
                                labels = parse_format())+
            facet_wrap(~cor_type,
                       nrow = n_row,
                       dir = direction,
                       scales = "free",
                       labeller = label_parsed) +
            geom_boxplot(data = accuracy_results_selected_dB,
                         aes(x = R, y = cor_coeff, group = R),
                         width=0.05, position = position_dodge(width=0.1),
                         outlier.alpha = 0,
                         colour = alpha('black', 0.75),
                         fill = alpha('grey', 0.25)) +
            geom_line(data = cor_df_sum_plt_df,
                      aes(x = R, y = cor_coeff, group = NA),
                      linewidth = 0.75,
                      colour = science_colours_six()[3])+
            geom_line(data = cor_df_sum_ref,
                      aes(x = R, y = cor_coeff, group = NA),
                      linewidth = 1,
                      linetype = "12",
                      colour = RgbToHex(c(100,100,100))) +
            geom_label(x = 0.25,
                       y = 0.85,
                       colour = 'black',
                       aes(label = paste("italic(MAE)=='", r_analogy_mean_txt, "'"), group = NA),
                       parse = TRUE,
                       family = .base_family,
                       data = annotion_df) +
            labs(x = "Conventional true value",
                 y = "Measured value") +
            guides(color = guide_legend(title = NULL, 
                                        title.theme = element_text(size = 1),
                                        nrow = n_row_legend,
                                        override.aes = list(alpha = 1)))+
            theme(legend.text.align = 0,
                  legend.position = "bottom",
                  legend.key.height = unit(0.15, 'cm'),
                  legend.text = element_text(size=6.5),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12)) -> p
    } else if(scenario_type == "Xks") {

        
        accuracy_results %>%
            as.data.frame() %>%
            mutate(dB = SNR_to_dB(SNR),
                   cor_coeff = abs(cor_coeff)) %>%
            group_by(cor_type, y_str, dB) %>%
            summarise(
                cor_coeff_Q3 = quantile(cor_coeff, 0.75, na.rm = TRUE),
                cor_coeff_Q1 = quantile(cor_coeff, 0.25, na.rm = TRUE),
                cor_coeff_median = mean(cor_coeff, na.rm = TRUE)) %>%
            pivot_longer(cols = c(
                cor_coeff_Q3,
                cor_coeff_Q1,
                cor_coeff_median),
                names_to = 'cor_coeff_type',
                values_to = 'cor_coeff') %>%
            as.data.frame() %>%
            mutate(cor_type = factor(cor_type,
                                     levels = supported_cor_types,
                                     labels = supported_cor_exprs)) %>%
            droplevels() -> accuracy_results2plt
        
        cor_df_sum_ref %>%
            mutate(R = dB_to_R(dB)) -> cor_df_sum_ref
        
        accuracy_results %>%
            as.data.frame() %>%
            mutate(dB = SNR_to_dB(SNR),
                   cor_coeff = abs(cor_coeff)) %>%
            dplyr::filter(near(dB, .dB_0.25) |
                              near(dB, .dB_0.50) |
                              near(dB, .dB_0.75) |
                              near(dB, .dB_1.00)) %>%
            mutate(cor_type = factor(cor_type,
                                     levels = supported_cor_types,
                                     labels = supported_cor_exprs)) %>%
            droplevels() %>%
            mutate(R = dB_to_R(dB)) -> accuracy_results_selected_dB
        
        
        pd <- position_dodge(0.1)
        
        accuracy_results2plt %>%
            group_by(cor_type, y_str, dB) %>%
            summarise(
                cor_coeff_Q3 = quantile(cor_coeff, 0.75, na.rm = TRUE),
                cor_coeff_Q1 = quantile(cor_coeff, 0.25, na.rm = TRUE),
                cor_coeff_median = median(cor_coeff, na.rm = TRUE)) %>%
            pivot_longer(cols = c(
                cor_coeff_Q3,
                cor_coeff_Q1,
                cor_coeff_median),
                names_to = 'cor_coeff_type',
                values_to = 'cor_coeff') %>%
            mutate(Ks = get_digits(y_str)) %>%
            mutate(R = dB_to_R(dB)) -> accuracy_results2plt2
        accuracy_results2plt2 %>%
            ggplot(aes(x = R, y = cor_coeff,  color = Ks, group = y_str)) +
            geom_line(aes(group = paste0(cor_coeff_type, y_str)), linewidth = 1.5) +
            scale_colour_gradient(low = anchor_colors[1],
                                  high = anchor_colors[2],
                                  labels = parse_format())+
            facet_wrap(~cor_type,
                       nrow = n_row,
                       dir = direction,
                       labeller = label_parsed) -> p_main
        
        p_main +
            geom_boxplot(data = accuracy_results_selected_dB,
                         aes(x = R, y = cor_coeff, group = R),
                         width=0.05, position = position_dodge(width=0.1),
                         outlier.alpha = 0,
                         colour = alpha('black', 0.75),
                         fill = alpha('grey', 0.25)) +
            geom_line(data = cor_df_sum_plt_df,
                      aes(x = R, y = cor_coeff, group = NA),
                      linewidth = 0.75,
                      colour = science_colours_six()[3])+
            geom_line(data = cor_df_sum_ref,
                      aes(x = R, y = cor_coeff, group = NA),
                      linewidth = 1,
                      linetype = "dotted",
                      colour = RgbToHex(c(100,100,100))) +
            geom_label(x = 0.35,
                       y = 0.85,
                       colour = 'black',
                       aes(label = paste("italic(MAE)=='", r_analogy_mean_txt, "'"), group = NA),
                       parse = TRUE,
                       family = .base_family,
                       data = annotion_df) +
            labs(x = "Conventional true value",
                 y = "Measured value",
                 color = expression(italic(k))) +
            theme(legend.text.align = 0) -> p
    }
    return(p)
}