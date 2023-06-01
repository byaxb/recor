#' Calculate the R2 for different nist_strd dataset
#'
#' @param dat_list_elemnt, component of the nist_strd, such as nist_strd[["Filip"]]
#' @details
#' Only the element of the nist_dat list is valid.
#' 
#' Currently, the following elements are supported:
#' 
#' \itemize{
#'   \item Filip
#'   \item NoInt1
#'   \item NoInt2
#'   \item Norris
#'   \item Pontius
#'   \item Wampler1
#'   \item Wampler2
#'   \item Wampler3
#'   \item Wampler4
#'   \item Wampler5
#'   \item Bennett5
#'   \item BoxBOD
#'   \item Chwirut1
#'   \item Chwirut2
#'   \item DanWood
#'   \item Eckerle4
#'   \item ENSO
#'   \item Gauss1
#'   \item Gauss2
#'   \item Gauss3
#'   \item Hahn1
#'   \item Kirby2
#'   \item Lanczos1
#'   \item Lanczos2
#'   \item Lanczos3
#'   \item MGH09
#'   \item MGH10
#'   \item MGH17
#'   \item Misra1a
#'   \item Misra1b
#'   \item Misra1c
#'   \item Misra1d
#'   \item Rat42
#'   \item Rat43
#'   \item Roszman1
#'   \item Thurber
#' }
#' 
#' 
#' More details for nist_strd are available at https://www.itl.nist.gov/div898/strd
#' 
#' @return R2 score
#' @export
cal_R2_nist_dat <- function(dat_list_elemnt) {
    param_df <- dat_list_elemnt$param
    for(i in 1:nrow(param_df)) {
        assign(param_df$Parameter[i], param_df$Estimate[i])
    }
    x <- dat_list_elemnt$data$x
    y_real <- dat_list_elemnt$data$y
    y_pred <- eval(parse(text = dat_list_elemnt$fun_expr))
    R2 <- 1 - sum((y_real - y_pred)^2) / sum((y_real - mean(y_real))^2)
    return(R2)
}