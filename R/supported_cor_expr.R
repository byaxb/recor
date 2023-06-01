#' supported cor types and their corresponding expressions
#'
#' @description
#' supported cor types and their corresponding expressions
#' @details
#' all supported cor types and their corresponding expressions
#' 
#' including those sharp ones
#' 
#' \tabular{ll}{
#'   \strong{cor_type} \tab \strong{cor_expr} \cr
#'   cor_ACE               \tab ACE                                  \cr
#'   cor_Ahat              \tab widehat(italic(A))                   \cr
#'   cor_bcor              \tab BCor                                 \cr
#'   sharp_bcor            \tab BCor^"#"                             \cr
#'   cor_bicor             \tab BiCor                                \cr
#'   sharp_bicor           \tab BiCor^"#"                            \cr
#'   cor_blomqvist         \tab Blomqvist*"'"*s~beta                 \cr
#'   cor_Ccor              \tab Ccor                                 \cr
#'   cor_CoS               \tab CoS                                  \cr
#'   cor_copent            \tab CopulaEntropy                        \cr
#'   cor_dcor              \tab dCor                                 \cr
#'   sharp_dcor            \tab dCor^"#"                             \cr
#'   cor_dcor_squared      \tab dCor^2                               \cr
#'   cor_dcor_rank         \tab dCor[italic(rank)]                   \cr
#'   cor_dhsic             \tab italic(d)*HSIC                       \cr
#'   sharp_dhsic           \tab italic(d)*HSIC^"#"                   \cr
#'   cor_gaussrank         \tab GaussRank                            \cr
#'   cor_gmic              \tab GMIC                                 \cr
#'   cor_Hellinger         \tab HellingerCor                         \cr
#'   cor_HHG               \tab HHG                                  \cr
#'   sharp_HHG             \tab HHG^"#"                              \cr
#'   cor_HHG_norm          \tab HHG_norm                             \cr
#'   cor_hoeffd            \tab Hoeffding*"'"*s~italic(D)            \cr
#'   cor_hsic              \tab HSIC                                 \cr
#'   cor_hsiccop           \tab italic(c)*HSIC                       \cr
#'   cor_identical         \tab italic(r)^symbol("-")                \cr
#'   cor_kendall           \tab Kendall*"'"*s~italic(tau)            \cr
#'   cor_mcd               \tab MCD                                  \cr
#'   cor_mgc               \tab MGC                                  \cr
#'   cor_mi                \tab MI                                   \cr
#'   cor_mic               \tab MIC                                  \cr
#'   cor_mic_sqrt          \tab italic(MIC)^frac(1, 2)               \cr
#'   cor_pearson           \tab Pearson*"'"*s~italic(r)              \cr
#'   sharp_pearson         \tab italic(r)^symbol("#")                \cr
#'   cor_pearson_squared   \tab italic(r)^2                          \cr
#'   cor_percentage_bend   \tab PbCor                                \cr
#'   sharp_percentage_bend \tab italic(r)[pb]^symbol("#")            \cr
#'   cor_quadrant          \tab QuadrantCor                          \cr
#'   cor_rdc               \tab RDC                                  \cr
#'   cor_RV                \tab RV                                   \cr
#'   sharp_RV              \tab RV^symbol("#")                       \cr
#'   cor_spearman          \tab Spearman*"'"*s~italic(rho)           \cr
#'   cor_TauStar           \tab Bergsma*"'"*s~italic(tau)^symbol("*")\cr
#'   sharp_TauStar         \tab italic(tau)^symbol("*"~"#")          \cr
#'   cor_tcor              \tab italic(r)^symbol("#")                \cr
#'   cor_tic               \tab TIC                                  \cr
#'   cor_win               \tab WinCor                               \cr
#'   sharp_win             \tab italic(r)[italic(w)]^symbol("#")     \cr
#'   cor_xicor             \tab Chatterjee*"'"*s~italic(xi)          \cr
#'   sharp_xicor           \tab italic(xi)^symbol("#")               \cr
#'   cor_xicor_c           \tab CODEC                                \cr
#'   r_prime               \tab italic(r)^symbol("-")                \cr
#'   loose_r               \tab italic(r)^symbol("-")                \cr
#'   looser_r              \tab italic(r)^symbol("-")                \cr
#'   zoom_pearson025       \tab zoom_pearson025                      
#' }
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
    


