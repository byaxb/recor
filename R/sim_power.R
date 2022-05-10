#' Power Simulation in parallel
#'
#' @description
#' Power Simulation
#' 
#' @param x_strs, the way to generate the independent variable x, with runif(512) as default
#' @param y_strs, y_strs, monotonic or non monotonic scenarios
#' @param cor_strs, cor_fun_strs, correlation statistics
#' @param SNRs, signal-to-noise ratio, with dB_to_SNR(-50:50) as default
#' @param nsim, sample length, with 500 as default
#' @param nround, number of simulation rounds, with 1 as default
#' @param ncores, cluster specification 
#' @details
#' x_strs: different sample size can be customized such as 
#' x_strs = paste0("runif", 10*50:100)
#' 
#' cor_strs: to calculate statistics such as dCor and MIC, energe and 
#' minerva should be loaded first
#' 
#' 
#' @return simulation results in a tidy data.frame
#' @export
sim_power <- function(x_strs = 'runif(512)',
                      y_strs = c("y <- x", 'y <- x^3'),
                      cor_strs = c("cor", "recor"),
                      SNRs = dB_to_SNR(-50:50),
                      nsim = 500,
                      nround = 1,
                      ncores = NULL,
                      ...) {
    conflict_prefer('%:%', 'foreach', quiet = TRUE)
    timeit('new')
    if(is.null(ncores)) {
        ncores <- parallel::detectCores() - 1
    }
    cl <- makeCluster(ncores)
    registerDoSNOW(cl)
    
    pb <- txtProgressBar(
        max =
            nround *
            nsim*
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
            cur_sim_idx = 1:nsim,
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
            alternative <- do.call(cur_cor, list(x = x, y = y))
            x <- eval_code_str(cur_x_str)
            null <- do.call(cur_cor, list(x = x, y = y))
            return(cbind(round_idx = cur_round_idx,
                         sim_idx = cur_sim_idx,
                         x_str = cur_x_str,
                         y_str = cur_y_str,
                         SNR = cur_SNR,
                         cor_type = cur_cor,
                         cor_alternative = alternative,
                         cor_null = null))
        }
    
    sim_results %>%
        as.data.frame() %>%
        mutate_at(vars(round_idx, sim_idx), as.integer) %>%
        mutate_at(vars(SNR, cor_alternative, cor_null), as.numeric) %>%
        mutate(SNRdB = SNR_to_dB(SNR)) %>%
        arrange(y_str, cor_type, x_str, SNRdB)-> sim_results
    stopCluster(cl)
    
    timeit('done')
    
    return(sim_results)
}