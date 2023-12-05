#' wrapper for scale_fill_manual function
#'
#' This is a wrapper function for the scale_fill_manual function. It uses pre defined a colorblind-friendly palette
#'
#' @param col_order if col_order is NULL (the default), the function uses the color by the default order, col_order argument is used to override this behavior.
#' @param ... Additional arguments passed to `scale_fill_manual`. Mainly names and labels (see example).
#' @references Based on <https://davidmathlogic.com/colorblind/#%230065A7-%23961d24-%23000000-%23E69F00-%2356B4E9-%23CC79A7-%23009988-%23BBBBBB>, <https://personal.sron.nl/~pault/>, and <https://paulgp.github.io/beamer_tips.pdf>
#' @details  This function is heavily inspired by ggplot's `scale_fill_brewer` function. The palette includes the follwoing colors: 1 - Blue ; 2 - Red ; 3 - Black ; 4 - Orange ; 5 - Sky Blue ; 6 - Yellow ; 7 - Reddish Purple ; 8 - Teal Green ; 9 - Grey.
#' @export
#' @examples
#' scale_fill_palette()
#' library(ggplot2)
#' bp <- ggplot(iris, aes(Species, Sepal.Length)) +
#' geom_boxplot(aes(fill = Species)) +
#' theme_minimal() +
#' theme(legend.position = "top")
#' bp # Regular colors by ggplot2
#' bp + scale_fill_palette2()
#' bp + scale_fill_palette2(index = c(2,5,3),labels=c("A","B","C"),name = "New Legend Title")
scale_fill_palette2<-function(index=NULL,...){
    require(ggplot2)
    mycol <- c("#0065A7","#961d24","#000000",
               "#E69F00","#56B4E9","#F0E442",
               "#CC79A7","#009988", "#999999")

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
    scale_fill_manual(values = vals,...)
}
