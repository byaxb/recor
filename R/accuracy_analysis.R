#' Basic function for accuracy analysis
#'
#' @description
#' To calculate the correlation scores in different scenarios at different noise levels 
#' @param x_strs, code string to generate x, with 'runif(512)' as default
#' @param y_strs, scenarios, with c("y <- x", 'y <- x^3') as default
#' @param cor_strs, correlation function names
#' @param SNRs, noise levels, with dB_to_SNR(-50:50) as default
#' @param nround, times of repetition, for robust estimation, with 10 as default
#' @details
#' The core function for accuracy analysis, 
#' usually as a workhorse called by accuracy_db()
#' 
#' 
#' Since the ground truth, which can be calculated on the basis of noise level, 
#' and the measured value are both returned,  
#' the result of this function can be further utilize 
#' to evaluate the accuracy for correlation statistics.
#' 
#' For efficiency, parallel computation is adopted here.
#' 
#' The algorithm automatically detects how many logical cores can be employed,
#' and parallel::detectCores() - 1 cores are actually used for this task.
#' 
#' @return a data.frame containing the correlation coefficient values of different types in different scenarios at different noise levels
#' @export
accuracy_analysis <- function(x_strs = 'runif(512)',
                          y_strs = c("y <- x", 'y <- x^3'),
                          cor_strs = c("loose_pearson", "cor_pearson", "sharp_pearson",
                                       "cor_spearman", "cor_kendall", "cor_dhsic", 
                                       "cor_dcor", "cor_mic", "cor_xicor"),
                          SNRs = dB_to_SNR(-50:50),
                          nround = 10,
                          ...) {
    conflict_prefer('%:%', 'foreach', quiet = TRUE)
    timeit('new')
    cl <- makeCluster(parallel::detectCores() - 1)
    registerDoSNOW(cl)
    
    pb <- txtProgressBar(
        max =
            nround *
            length(x_strs) *
            length(SNRs) *
            length(y_strs) *
            length(cor_strs),
        style = 3
    )
    progress <- function(n)
        setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    sim_results <-
        foreach(cur_round_idx = 1:nround,
                .multicombine = TRUE,
                .combine = "rbind",
                .inorder = TRUE,
                .packages = c("recor")
        ) %:%
        foreach(
            cur_x_str = x_strs,
            .multicombine = TRUE,
            .combine = "rbind",
            .inorder = TRUE,
            .packages = c("recor")
        ) %:%
        foreach(
            cur_SNR = SNRs,
            .multicombine = TRUE,
            .combine = "rbind",
            .inorder = TRUE,
            .packages = c("recor")
        ) %:%
        foreach(
            cur_y_str = y_strs,
            .multicombine = TRUE,
            .combine = "rbind",
            .inorder = TRUE,
            .packages = c("recor")
        ) %:%
        foreach(
            cur_cor = cor_strs,
            .multicombine = TRUE,
            .combine = "rbind",
            .inorder = TRUE,
            .options.snow = opts,
            .packages = c("recor")
        )%dopar% {
            x <- eval_code_str(cur_x_str)
            y_hat <- eval_code_str(cur_y_str)
            y <- get_noised_y(y_hat, cur_SNR)
            cor_coeff <- do.call(cur_cor, list(x = x, y = y))
            return(cbind(round_idx = cur_round_idx,
                         x_str = cur_x_str,
                         y_str = cur_y_str,
                         SNR = cur_SNR,
                         cor_type = cur_cor,
                         cor_coeff = cor_coeff))
        }
    
    stopCluster(cl)
    
    sim_results %>%
        as.data.frame() %>%
        mutate(round_idx = as.integer(round_idx)) %>%
        mutate_at(vars(SNR, cor_coeff), as.numeric) %>%
        mutate(SNRdB = SNR_to_dB(SNR)) %>%
        arrange(x_str, y_str, cor_type, SNRdB, round_idx)-> sim_results
    
    timeit('done')
    
    return(sim_results)
}



