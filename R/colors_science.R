#' Colors popular in Science
#'
#' @description
#' Colors popular in the Science
#' @usage
#' science_colours_six()

#' @return a string vector

#' @export
science_colours_six <- function() {
    rgb_colors <- list(r_prime = c(91,183,206),
                       r = c(84,172,117),
                       r_sharp = c(197,87,90),
                       rho = c(55,142,205),
                       dCor = c(202,180,122),
                       MIC = c(117,114,181))
    hex_colors <- sapply(rgb_colors, RgbToHex)
    return(as.character(hex_colors))
}

#' @export
.anchor_colors <- as.character(science_colours_six()[c(1,2,3)])
