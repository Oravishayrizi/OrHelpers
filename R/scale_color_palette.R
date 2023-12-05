#' wrapper for scale_color_manual function
#'
#' This is a wrapper function for the scale_color_manual function. It uses pre defined a colorblind-friendly palette
#'
#' @param col_order if col_order is NULL (the default), the function uses the color by the default order, col_order argument is used to override this behavior.
#' @param ... Additional arguments passed to `scale_color_manual`. Mainly names and labels (see example).
#' @references Based on <https://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette>, and <https://paulgp.github.io/beamer_tips.pdf>
#' @details  This function is heavily inspired by ggplot's `scale_fill_brewer` function. The palette includes the follwoing colors: 1 - Blue ; 2 - Vermilion ; 3 - Bluish Green ; 4 - Orange ; 5 - Sky Blue ; 6 - Yellow ; 7 - Reddish Purple ; 8 - Grey.
#' @export
#' @examples
#' library(ggplot2)
#' bp <- ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length,color = Species)) +
#' geom_point(size=5) +
#' theme_minimal() +
#' theme(legend.position = "top")
#' bp # Regular colors by ggplot2
#' bp + scale_color_palette()
#' bp + scale_color_palette(index = c(4,5,7),labels=c("A","B","C"),name = "New Legend Title")
scale_color_palette<-function(index=NULL,...){
    require(ggplot2)

    mycol <- c("#0072B2","#D55E00","#009E73","#E69F00","#56B4E9","#F0E442","#CC79A7", "#999999")

    if (!is.numeric(index) & !is.null(index)) {
        stop("index should be either a vector of integers or NULL")
    }
    if (is.null(index)) {
        vals<-mycol
    } else{ # If index is numeric vector

        if (is.numeric(index)) {
            vals<-mycol[index]
        }
        if (length(index)>length(mycol)) {
            warning("index length is too large, allowed maximum for the palette is 8 Returning the palette with that many colors")
            vals<-mycol
        }
        if (max(index)>length(mycol) | min(index)<1  ) {
            warning("one or more value of `index` are not in the range of 1-8, the colors for these labels will be Black")
        }

        if (sum(floor(index)==index)!=length(index)) {
            warning("`index` should be either a vector of integers or NULL")
        }
    }
    scale_color_manual(values = vals,...)
}
