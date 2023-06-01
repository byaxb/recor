#' Colors
#'
#' @description
#' Colors
#' @details
#' Some colors used in paper: 
#' https://arxiv.org/abs/2205.04571
#'
#' @return colors
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
science_colours_five <- function() {
    rgb_colors <- list(c(1, 86, 153),
                       c(250, 192, 15),
                       c(243, 118, 74),
                       c(95, 198, 201),
                       c(79, 89, 109))
    hex_colors <- sapply(rgb_colors, RgbToHex)
    return(as.character(hex_colors))
}

#' @export
science_colours_four <- function() {
    rgb_colors <- list(c(23, 23, 23),
                       c(6, 233, 6),
                       c(255, 28, 0),
                       c(0, 37, 255))
    hex_colors <- sapply(rgb_colors, RgbToHex)
    return(as.character(hex_colors))
}

#' @export
science_colours_three_I <- function() {
    rgb_colors <- list(c(77, 133, 189),
                       c(247, 144, 61),
                       c(89, 169, 90))
    hex_colors <- sapply(rgb_colors, RgbToHex)
    return(as.character(hex_colors))
}

#' @export
science_colours_three_II <- function() {
    rgb_colors <- list(c(210, 32, 39),
                       c(56, 89, 137),
                       c(127, 165, 183))
    hex_colors <- sapply(rgb_colors, RgbToHex)
    return(as.character(hex_colors))
}

#' @export
science_colours_interpolation <- function(n = 3, anchor_colors = NULL) {
    if(is.null(anchor_colors)) {
        anchor_colors <- as.character(science_colours_six()[c(1,2,3)])
    }
    ncolors <- colorRampPalette(anchor_colors)(n)
    return(as.character(ncolors))
}


#' @export
.anchor_colors <- as.character(science_colours_six()[c(1,2,3)])

#' @export
science_colours <- function(n = 3, anchor_colors = NULL) {
    if(is.null(anchor_colors)) {
        anchor_colors <- .anchor_colors[1:2]
    }
    ncolors <- colorRampPalette(anchor_colors)(n)
    return(as.character(ncolors))
}
