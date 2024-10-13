#' To get the index of A in B
#'
#' @param A, a vector
#' @param B, a vector
#' @details
#' A wrapper for match(A, B)
#' @return the index of A in B
#' @export
index_of_A_in_B <- function(A, B) {
    match(A, B)
}
