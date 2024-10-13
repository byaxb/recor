#' Supported cor types and their corresponding expressions
#'
#' @description
#' supported cor types and their corresponding expressions
#' 
#' @return data.frame, one column as cor_type, the other as cor_expr
#' @export
supported_cor_expr <- function() {
    expr_df <- as.data.frame(
        matrix(c(
            "cor_ACE", "ACE",
            "cor_Ahat", "widehat(italic(A))",
            "cor_bcor", "BallCor",
            "sharp_bcor", "BCor^\"#\"",
            "cor_bicor", "BiCor",
            "sharp_bicor", "BiCor^\"#\"",
            "cor_blomqvist", "Blomqvist*\"'\"*s~beta",
            "cor_Ccor", "Ccor",
            "cor_CoS", "CoS",
            "cor_copent", "CopulaEntropy",
            "cor_dcor", "dCor",
            "sharp_dcor", "dCor^\"#\"",
            "cor_dcor_squared", "dCor^2",
            "cor_dcor_rank", "dCor[italic(rank)]",
            "cor_dhsic", "italic(d)*HSIC",
            "sharp_dhsic", "italic(d)*HSIC^\"#\"",
            "cor_gaussrank", "GaussRank",
            "cor_gmic", "GMIC",
            "cor_Hellinger", "HellingerCor",
            "cor_HHG", "HHG",
            "sharp_HHG", "HHG^\"#\"",
            "cor_HHG_norm", "HHG_norm", #HHG will be normalized by default
            "cor_hoeffd", "Hoeffding*\"'\"*s~italic(D)",
            "cor_hsic", "HSIC",
            "cor_hsiccop", "italic(c)*HSIC", #copula
            "cor_identical", "italic(r)^symbol(\"-\")",
            "cor_kendall", "Kendall*\"'\"*s~italic(tau)",
            "cor_mcd", "MCD",
            "cor_mgc", "MGC",
            "cor_mi", "MI",
            "cor_mic", "MIC",
            "cor_mic_sqrt", "italic(MIC)^frac(1, 2)",
            "cor_pearson", "Pearson*\"'\"*s~italic(r)",
            "sharp_pearson", "italic(r)^symbol(\"#\")",
            "cor_pearson_squared", "italic(r)^2",
            "cor_percentage_bend", "PbCor",
            "sharp_percentage_bend", "italic(r)[pb]^symbol(\"#\")",
            "cor_quadrant", "QuadrantCor",
            "cor_rdc", "RDC",
            "cor_RV", "RV",
            "sharp_RV", "RV^symbol(\"#\")",
            "cor_spearman", "Spearman*\"'\"*s~italic(rho)",
            "cor_TauStar", "Bergsma*\"'\"*s~italic(tau)^symbol(\"*\")",
            "sharp_TauStar","italic(tau)^symbol(\"*\"~\"#\")",
            "cor_tcor", "italic(r)^symbol(\"#\")",
            "cor_tic", "TIC",
            "cor_win", "WinCor",
            "sharp_win","italic(r)[italic(w)]^symbol(\"#\")",
            "cor_xicor", "Chatterjee*\"'\"*s~italic(xi)",
            "sharp_xicor","italic(xi)^symbol(\"#\")",
            "cor_xicor_c", "CODEC",
            "r_prime", "italic(r)^symbol(\"-\")",
            "loose_pearson", "italic(r)^symbol(\"-\")",
            "looser_pearson", "italic(r)^symbol(\"-\")",
            "zoom_pearson025", "zoom_pearson025"
            ),
            byrow = TRUE,
            ncol = 2
        ))
    names(expr_df) <- c("cor_type", "cor_expr")
    return(expr_df)
}
#' @export
cor_type2expr <- function(cor_type) {
    factor(as.character(cor_type),
           levels = supported_cor_expr()$cor_type,
           labels = supported_cor_expr()$cor_expr)
}
    


