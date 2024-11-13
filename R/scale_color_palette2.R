#' Color Scale for Colorblind-Friendly Palette
#'
#' `scale_color_palette` provides a color scale that uses a colorblind-friendly palette, wrapping the `scale_color_manual` function.
#'
#' @description
#' This function applies a predefined colorblind-friendly palette to color aesthetics in `ggplot2` plots. You can select specific colors from the palette by specifying the `index` parameter.
#'
#' The default palette includes the following colors:
#' 1 - Navy Blue, 2 - Crimson, 3 - Forest Green, 4 - Gold, 5 - Coral, 6 - Lavender, 7 - Teal, 8 - Dark Grey.
#'
#' @param index A numeric vector specifying the indices of colors to use from the predefined alternative palette. Values must be between 1 and 8. If NULL (the default), all colors are used.
#' @param ... Additional arguments passed to `scale_color_manual` or `scale_fill_manual`, such as `name` for legend title, or `labels` for legend labels.
#' @references Based on <https://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette> and <https://paulgp.github.io/beamer_tips.pdf>.
#' @details The functions use an alternative predefined color palette specifically designed to be distinguishable for colorblind users.
#' @examples
#' library(ggplot2)
#' bp <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
#'   geom_point(size = 5)
#' bp + scale_color_palette2()
#'
#' bp + scale_color_palette2(index = c(4, 5, 7),
#'  labels = c("A", "B", "C"),
#'   name = "New Legend Title")
#'
#' @export
scale_color_palette2 <- function(index = NULL, ...) {
    mycol <- c("#1B3B6F", "#B2182B", "#238B45", "#FFD700",
               "#FF7F50", "#E6E6FA", "#008080", "#404040")

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
    ggplot2::scale_color_manual(values = vals, ...)
}
