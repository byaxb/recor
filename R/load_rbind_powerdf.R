#' Load and rbind the powerdf rda files
#' @description
#' load several powerdf rda files all at once and rbind the power_df into a list
#' @param pdir, the path storing the powerdf rda files
#' @details
#' 
#' load_all() is called, and a list including three parts, power_df.90, .95 and .99 will be returned
#' 
#' @return a list
#' @export
load_rbind_powerdf <- function(pdir = NULL) {
    files_to_be_loaded <- list.files(
        pdir,
        pattern = "^power_df",
        full.names = TRUE)
    obj_list <- load_all(files_to_be_loaded)
    power_df.90 <- do.call("rbind", obj_list[str_which(names(obj_list), "90$") ])
    power_df.95 <- do.call("rbind", obj_list[str_which(names(obj_list), "95$") ])
    power_df.99 <- do.call("rbind", obj_list[str_which(names(obj_list), "99$") ])
    return(list(power_df.90 = power_df.90,
                power_df.95 = power_df.95,
                power_df.99 = power_df.99))
}
