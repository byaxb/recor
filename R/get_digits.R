#' Extract the digital parts from a character vector
#'
#' @description
#' Extract the digital parts from a character vector
#' @param x, a character vector
#' @details
#' 
#' as.numeric(str_extract(x, "\\d+\\.?\\d{0,}"))
#'
#' @return a numeric vector
#' @export
get_digits <- function(x) {
    #as.numeric(str_extract(x, "[[:digit:]]+\\.?[[:digit:]]+"))
    as.numeric(str_extract(x, "\\d+\\.?\\d{0,}"))
}
