#' Mostly used correlation functions
#'
#' @details
#' two_cor_funs(): 'cor_spearman' and 'cor_tcor'  
#' 
#' three_cor_funs(): 'cor_pearson', 'cor_spearman', and 'cor_tcor'  
#' 
#' three_cor_funs2(): 'r_prime', 'r_pearson', and 'r_sharp'  
#' 
#' four_cor_funs(): 'cor_pearson', 'cor_spearman', 'cor_kendall', and 'cor_tcor'  
#' 
#' six_cor_funs(): cor_pearson,cor_spearman,cor_kendall,cor_tcor,cor_dcor, cor_mic  
#' 
#' six_cor_funs2(): r_prime, cor_pearson, cor_spearman, cor_tcor, cor_dcor, cor_mic  
#' 
#' seven_cor_funs()：union(six_cor_funs(), six_cor_funs2())
#' @return 2-7 correlaiton names
#' @export
two_cor_funs <- function() {
    return(c('cor_spearman',
             'cor_tcor'))
}
#' @export
three_cor_funs <- function() {
    return(c('cor_pearson', 
             'cor_spearman',
             'cor_tcor'))
}
#' @export
three_cor_funs2 <- function() {
    return(c('r_prime',
             'cor_pearson',
             'cor_tcor'))
}
#' @export
four_cor_funs <- function() {
    return(c('cor_pearson', 
             'cor_spearman',
             'cor_kendall',
             'cor_tcor'))
}
#' @export
six_cor_funs <- function() {
    return(c('cor_pearson', 
             'cor_spearman',
             'cor_kendall',
             'cor_tcor',
             'cor_dcor',
             'cor_mic'))
}
#' @export
six_cor_funs2 <- function() {
    return(c('r_prime',
             'cor_pearson', 
             'cor_spearman',
             'cor_tcor',
             'cor_dcor',
             'cor_mic'))
}

#' @export
nine_cor_funs <- function() {
    return(c('cor_pearson', 
             'cor_spearman',
             'cor_kendall',
             'cor_hoeffd',
             'cor_tcor',
             'cor_rdc',
             'cor_mi',
             'cor_mic',
             'cor_dcor'))
}


#' @export
seven_cor_funs <- function() {
    return(c('r_prime',
             'cor_pearson', 
             'cor_spearman',
             'cor_kendall',
             'cor_tcor',
             'cor_dcor',
             'cor_mic'))
}


#' @export
cor_funs_level_label <- function() {
    cor_funs_expression <- c(
        'r_prime','italic(r)^symbol("\055")',
        'r_minus','italic(r)^symbol("\055")',
        'r_arithmetic','italic(r)^symbol("\055")',
        'cor_pearson','italic(r)^symbol("\040")',
        'r_pearson','italic(r)^symbol("\040")',
        'r_geometric','italic(r)^symbol("\040")',
        'r_plus','italic(r)^symbol("\053")',
        'cor_tcor2', 'italic(r)^symbol("\053")',
        'cor_tcor','italic(r)^symbol("#")',
        'cor_rsharp','italic(r)^symbol("#")',
        'sharp_pearson','italic(r)^symbol("#")',
        'r_sharp','italic(r)^symbol("#")',
        'r_rearrangement','italic(r)^symbol("#")',
        'cor_tcor_pearson','italic(r)^symbol("#")',
        'cor_spearman','italic(rho)',
        'sharp_spearman','italic(rho)^symbol("#")',
        'cor_tcor_spearman','italic(rho)^symbol("#")',
        'cor_dcor','dCor',
        'dcor','dCor',
        'cor_tcor_D2','italic(r)^symbol("\053")',
        'sharp_dcor','dCor^symbol("#")',
        'cor_tcor_dcor','dCor^symbol("#")',
        'cor_tcor_D','dCor^symbol("#")',
        'cor_mic','MIC',
        'cor_gmic','MIC[italic(g)]',
        'cor_tic', 'TIC',
        'cor_kendall','italic(tau)',
        'cor_ball','BCor',
        'cor_bcor','BCor',
        'bcor','BCor',
        "sharp_bcor",'BCor^symbol("#")',
        'cor_dcor_idx2','dCor_idx2',
        'cor_hoeffd','italic(D)',
        'cor_hsic','dHSIC',
        'cor_mi','MI',
        'cor_mm','Min-Max',
        'cor_rdc','RDC',
        'sharp_cosine','cos^symbol("#")'
    )
    
    cor_funs_level_label_matrix <- matrix(cor_funs_expression,
        byrow = TRUE, ncol = 2)
    cor_funs_level_label_matrix %>%
        as.data.frame() %>%
        set_names(c("level", 'label')) -> cor_funs_level_label_df
    return(cor_funs_level_label_df)
}


#' @export
selected_cor_funs <- function() {
    cor_funs <- c(
        "r_prime",
        "r_minus",
        "r_arithmetic",
        "cor_pearson",
        "r_pearson",
        "r_geometric",
        "r_plus",
        "cor_tcor2",
        "cor_tcor",
        "cor_rsharp",
        "sharp_pearson",
        "r_sharp",
        "r_rearrangement",
        "cor_tcor_pearson",
        "cor_spearman",
        "sharp_spearman",
        "cor_tcor_spearman",
        "cor_dcor",
        "dcor",
        "cor_tcor_D2",
        "sharp_dcor",
        "cor_tcor_dcor",
        "cor_tcor_D",
        "cor_mic",
        "cor_gmic",
        "cor_tic",
        "cor_kendall",
        "cor_ball",
        "cor_bcor",
        "bcor",
        "sharp_bcor",
        "cor_dcor_idx2",
        "cor_hoeffd",
        "cor_hsic",
        "cor_mi",
        "cor_mm",
        "cor_rdc",
        "sharp_cosine"
    )
    expressions_bak <- c(
        "italic(r)^symbol(\"-\")",
        "italic(r)^symbol(\"-\")",
        "italic(r)^symbol(\"-\")",
        "italic(r)^symbol(\" \")",
        "italic(r)^symbol(\" \")",
        "italic(r)^symbol(\" \")",
        "italic(r)^symbol(\"+\")",
        "italic(r)^symbol(\"+\")",
        "italic(r)^symbol(\"#\")",
        "italic(r)^symbol(\"#\")",
        "italic(r)^symbol(\"#\")",
        "italic(r)^symbol(\"#\")",
        "italic(r)^symbol(\"#\")",
        "italic(r)^symbol(\"#\")",
        "italic(rho)",
        "italic(rho)^symbol(\"#\")",
        "italic(rho)^symbol(\"#\")",
        "dCor",
        "dCor",
        "italic(r)^symbol(\"+\")",
        "dCor^symbol(\"#\")",
        "dCor^symbol(\"#\")",
        "dCor^symbol(\"#\")",
        "MIC",
        "MIC[italic(g)]",
        "TIC",
        "italic(tau)",
        "BCor",
        "BCor",
        "BCor",
        "BCor^symbol(\"#\")",
        "dCor_idx2",
        "italic(D)",
        "dHSIC",
        "MI",
        "Min-Max",
        "RDC",
        "cos^symbol(\"#\")" 
    )
    expressions <- c(
        "italic(r)^symbol(\"-\")",
        "italic(r)^symbol(\"-\")",
        "italic(r)^symbol(\"-\")",
        "Pearson*\"'\"*s~italic(r)^symbol(\" \")",
        "Pearson*\"'\"*s~italic(r)^symbol(\" \")",
        "Pearson*\"'\"*s~italic(r)^symbol(\" \")",
        "italic(r)^symbol(\"+\")",
        "italic(r)^symbol(\"+\")",
        "italic(r)^symbol(\"#\")",
        "italic(r)^symbol(\"#\")",
        "italic(r)^symbol(\"#\")",
        "italic(r)^symbol(\"#\")",
        "italic(r)^symbol(\"#\")",
        "italic(r)^symbol(\"#\")",
        "Spearman*\"'\"*s~italic(rho)",
        "italic(rho)^symbol(\"#\")",
        "italic(rho)^symbol(\"#\")",
        "dCor",
        "dCor",
        "italic(r)^symbol(\"+\")",
        "dCor^symbol(\"#\")",
        "dCor^symbol(\"#\")",
        "dCor^symbol(\"#\")",
        "MIC",
        "MIC[italic(g)]",
        "TIC",
        "Kendall*\"'\"*s~italic(tau)",
        "BCor",
        "BCor",
        "BCor",
        "BCor^symbol(\"#\")",
        "dCor_idx2",
        "Hoeffding*\"'\"*s~italic(D)",
        "dHSIC",
        "MI",
        "Min-Max",
        "RDC",
        "cos^symbol(\"#\")" 
    )
    
    return(list(cor_funs = cor_funs,
                expressions = expressions))
}


#' @export
.selected_cor_funs <- selected_cor_funs()$cor_funs

#' @export
.selected_cor_funs_expressions <- selected_cor_funs()$expressions
