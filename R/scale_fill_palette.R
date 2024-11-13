#' Fill Scale for Colorblind-Friendly Palette
#'
#' `scale_fill_palette` provides a fill scale that uses a colorblind-friendly palette, wrapping the `scale_fill_manual` function.
#'
#' @description
#' This function applies a predefined colorblind-friendly palette to fill aesthetics in `ggplot2` plots. You can select specific colors from the palette by specifying the `index` parameter.
#'
#' The default palette includes the following colors:
#' 1 - Blue, 2 - Vermilion, 3 - Bluish Green, 4 - Orange, 5 - Sky Blue, 6 - Yellow, 7 - Reddish Purple, 8 - Grey.
#'
#' @param index A numeric vector specifying the indices of colors to use from the predefined palette. Values must be between 1 and 8. If NULL (the default), all colors are used.
#' @param ... Additional arguments passed to `scale_fill_manual`, such as `name` for legend title, or `labels` for legend labels.
#' @return A ggplot2 fill scale function with the colorblind-friendly palette.
#' @references Based on <https://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette> and <https://paulgp.github.io/beamer_tips.pdf>.
#' @examples
#' library(ggplot2)
#' bp_fill <- ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
#'   geom_bar()
#' bp_fill + scale_fill_palette()
#'
#' bp_fill + scale_fill_palette(index = c(3, 5, 1),
#'  labels = c("Species A", "Species B", "Species C"),
#'   name = "Iris Species")
#'
#' @export
scale_fill_palette <- function(index = NULL, ...) {
    mycol <- c("#0072B2", "#D55E00", "#009E73", "#E69F00",
               "#56B4E9", "#F0E442", "#CC79A7", "#999999")

    if (!is.numeric(index) && !is.null(index)) {
        stop("index should be either a vector of integers or NULL")
    }
    if (is.null(index)) {
        vals <- mycol
    } else {
        vals <- mycol[index]
        if (length(index) > length(mycol)) {
            warning("index length is too large; returning full palette.")
            vals <- mycol
        }
        if (max(index) > length(mycol) || min(index) < 1) {
            warning("Some values of `index` are out of range (1-8); setting these to black.")
            vals <- replace(mycol, !(index %in% 1:8), "black")
        }
    }
    ggplot2::scale_fill_manual(values = vals, ...)
}
