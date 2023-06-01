#' Basic function for power analysis
#'
#' @description
#' Basic function for power analysis
#' @param x_strs, the way to generate the independent variable x, with runif(512) as default
#' @param y_strs, y_strs such as y <- x
#' @param cor_strs, cor_fun_strs, with the returns of three_cor_funs() as default
#' @param SNRs, signal-to-noise ratio, with dB_to_SNR(-50:50) as default
#' @param nsim, times of null simulation, with 500 as default
#' @details
#' The core function for power analysis, 
#' usually as a workhorse called by power_db()
#' 
#' For efficiency, parallel computation is adopted here.
#' 
#' The algorithm automatically detects how many logical cores can be employed,
#' and parallel::detectCores() - 1 cores are actually used for this task.
#' 
#' @return simulation results in a data.frame
#' @import byaxb
#' @import parallel
#' @export
sim_power <- function(x_strs = 'runif(512)',
                      y_strs = c("y <- x", 'y <- x^3'),
                      cor_strs = three_cor_funs(),
                      SNRs = dB_to_SNR(-50:50),
                      nsim = 500,
                      nround = 1,
                      err_log = "err_log.rda",
                      ...) {
    conflict_prefer('%:%', 'foreach', quiet = TRUE)
    conflict_prefer('detectCores', 'parallel', quiet = TRUE)
    timeit('new')
    cl <- makeCluster(detectCores() - 1)
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
                .packages = c("recor", "byaxb")
        ) %:%
        foreach(
            cur_sim_idx = 1:nsim,
            .multicombine = TRUE,
            .combine = "rbind",
            .inorder = TRUE,
            .packages = c("recor", "byaxb")
        ) %:%
        foreach(
            cur_x_str = x_strs,
            .multicombine = TRUE,
            .combine = "rbind",
            .inorder = TRUE,
            .packages = c("recor", "byaxb")
        ) %:%
        foreach(
            cur_SNR = SNRs,
            .multicombine = TRUE,
            .combine = "rbind",
            .inorder = TRUE,
            .packages = c("recor", "byaxb")
        ) %:%
        foreach(
            cur_y_str = y_strs,
            .multicombine = TRUE,
            .combine = "rbind",
            .inorder = TRUE,
            .packages = c("recor", "byaxb")
        ) %:%
        foreach(
            cur_cor = cor_strs,
            .multicombine = TRUE,
            .combine = "rbind",
            .inorder = TRUE,
            .options.snow = opts,
            .packages = c("recor", "byaxb")
        )%dopar% {
            cur_cell <- tryCatch(expr = {
                x <- eval_code_str(cur_x_str)
                y_hat <- eval_code_str(cur_y_str)
                y <- get_noised_y(y_hat, cur_SNR)
                y <- as.numeric(y)
                alternative <- do.call(cur_cor, list(x = x, y = y))
                x <- eval_code_str(cur_x_str)
                null <- do.call(cur_cor, list(x = x, y = y))
                data.frame(
                    round_idx = cur_round_idx,
                    sim_idx = cur_sim_idx,
                    x_str = cur_x_str,
                    y_str = cur_y_str,
                    SNR = cur_SNR,
                    cor_type = cur_cor,
                    cor_alternative = alternative,
                    cor_null = null)
            }, error = function(err) {
                #save error status
                save(
                    cur_round_idx, cur_sim_idx, 
                    cur_x_str, cur_y_str, 
                    cur_SNR, cur_cor,
                    x, y_hat, y,
                    file = add_time_ext(err_log))
                #return NA
                data.frame(
                    round_idx = cur_round_idx,
                    sim_idx = cur_sim_idx,
                    x_str = cur_x_str,
                    y_str = cur_y_str,
                    SNR = cur_SNR,
                    cor_type = cur_cor,
                    cor_alternative = NA,
                    cor_null = NA)
            })
            return(cur_cell)
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