#' Load data.frame objects in different .rda files and rbind them together
#'
#' @param dir, destination directory for rda files
#' @details
#' if more than one data.frame objects are stored in some .rda file, 
#' only the first one will be loaded, and the remaining ones are omitted
#' @return a long data.frame
#' @export
load_rbind <- function(dir = NULL) {
    files_to_be_loaded <- list.files(dir, full.names = TRUE)
    loaded_objs <- list()
    for (cur_file in files_to_be_loaded) {
        obj_name <- load(cur_file)
        loaded_objs[[cur_file]] <- get(obj_name[1])
    }
    all_df <- as.data.frame(do.call("rbind", loaded_objs))
    row.names(all_df) <- NULL
    return(all_df)
}