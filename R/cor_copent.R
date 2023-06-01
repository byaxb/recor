#' Estimating copula entropy
#'
#' @description
#' A wrapper for copent::copent()
#' @param x, numeric vector
#' @param y, numeric vector
#' @details
#' See help file for copent::copent
#'
#' @return The negative value of copula entropy of data x
#' @references 
#' Ma, J., & Sun, Z. (2011). Mutual information is copula entropy. 
#' Tsinghua Science & Technology, 16(1): 51-54. 
#' See also arXiv preprint arXiv:0808.0845, 2008.
#' @import copent
#' @export
cor_copent  <- function(x, y) {
    cor_value <- tryCatch(expr = {
        copent(data.frame(x, y))
    }, error = function(err) {NA})
    return(cor_value)
}
