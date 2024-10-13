#' Load data.table objects in different .rda files and rbind them together
#'
#' @param dir, destination directory for rda files
#' @param ..., more parameters to be passed to list.files, such as pattern
#' @details
#' if more than one data.frame objects are stored in some .rda file, 
#' only the first one will be loaded, and the remaining ones are omitted
#' @return a long data.table (data.frame)
#' @import data.table
#' @export
load_rbind.data.table <- function (dir = NULL, ...) {
    files_to_be_loaded <- list.files(dir, full.names = TRUE, ...)
    cat("\nObjects from ", length(files_to_be_loaded), " files will be rbinded\n\n")
    print(files_to_be_loaded)
    loaded_objs <- list()
    pbfor::pb_for()
    for (cur_file in files_to_be_loaded) {
        obj_name <- load(cur_file)
        loaded_objs[[cur_file]] <- as.data.table(get(obj_name[1])) 
    }
    all_df <- rbindlist(loaded_objs)
    return(all_df)
}
