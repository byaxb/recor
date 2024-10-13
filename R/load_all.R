#' Load all objects in several .rda files into a whole list
#'
#' @param files_to_be_loaded, several file paths
#' @details
#' all the objects are loaded into a list, 
#' each as a component.
#' 
#' For example, if there are 3 objects in file1 and 4 objects in file2,
#' then the resulted list will contain all these 7 objects
#' @return a list, consisting all the loaded objects
#' @export
load_all <- function(files_to_be_loaded) {
    loaded_objs <- list()
    obj_names_all <- NULL
    idx <- 0
    for(cur_file in files_to_be_loaded) {
        obj_names <- load(cur_file)
        for(obj_name in obj_names) {
            idx <- idx + 1
            loaded_objs[[idx]] <- get(obj_name) 
        }
        obj_names_all <- c(obj_names_all, obj_names)
    }
    names(loaded_objs) <- obj_names_all
    return(loaded_objs)
}

