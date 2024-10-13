#' Add time information, often as version, to arbitrary file name
#'
#' @param filename, a file name string
#' @param type, accurate to day, min or second, with second as default
#' @details
#' add time information to distinguish different versions of files
#' @return new file name with time information
#' @export
add_time_ext <- function(filename, type = c('sec', 'min', 'day')) {
    type <- match.arg(type)
    base_filename <- tools::file_path_sans_ext(filename)
    ext_filename <- tools::file_ext(filename)
    rectified_filename <- paste0(base_filename, "_", sys_time_str(type))
    if(length(ext_filename) >= 1) {
        rectified_filename <- paste0(rectified_filename, '.', ext_filename)
    }
    return(rectified_filename)
}

