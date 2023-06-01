#' Heller-Heller-Gorfine (HHG) Tests of Independence
#'
#' @description
#' A wrapper for HHG::HHG()
#' @param x, numeric vector
#' @param y, numeric vector
#' @param normalized, whether to normalized HHG to scale [0, 1], with TRUE as default
#' @details
#' See help file for HHG::HHG
#' 
#' Official package HHG can be installed from CRAN or 
#' http://www.math.tau.ac.il/~ruheller/Software.html
#' 
#' Same code lines can also be found in the following site: 
#' https://github.com/soumikp/fastMI/blob/master/code/functions.R.gz
#'
#' @return correlation coefficients
#' @import HHG
#' @export
cor_HHG <- function(x, y, normalized = TRUE) {
    HHG <- function(x, y) {
        cor_value <- tryCatch(expr = {
            Dx = as.matrix(dist((x), diag = TRUE, upper = TRUE))
            Dy = as.matrix(dist((y), diag = TRUE, upper = TRUE))
            hhg.test(Dx, Dy, nr.perm = 0)$sum.chisq
            #Fast.independence.test(x, y, nr.perm=0)$MinP
        }, error = function(err) {NA})
        return(cor_value)
    }
    if(normalized) {
        return(HHG(x, y) / sqrt(HHG(x, x) * HHG(y, y)))
    } else {
        return(HHG(x, y))
    }
}
