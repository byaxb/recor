#' Wrapper for eval code strings
#'
#' @param code_str, string to be evaluated
#' @details
#' A wrapper for eval code strings
#' envir is set as parent.frame()
#' 
#' For internal use in sim_power()
#' 
#' @return The results of code_str as code
#' @export
eval_code_str <- function(code_str) {
    eval(parse(text = code_str), envir = parent.frame())
}

