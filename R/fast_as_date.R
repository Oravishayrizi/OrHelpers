#' Fast change to Date class
#'
#' This function coverts a character or factor vector into a vector of dates.
#'
#' @param x a vector to be converted into date.
#' @param ... Additional arguments passed to `as.Date.character`.
#' @return A vector of Dates
#' @export
#' @references Based on <https://towardsdatascience.com/tricks-in-r-to-boost-your-productivity-8c977242c69c>
#' @examples
#' fast_as_date(c("2020-10-15","2020-12-03"))
#' fast_as_date(c("10/15/20","12/03/20"),format='%m/%d/%y')
fast_as_date<-function(x,...){
    if (is.factor(x)) {
        x <- as.character(x)
    }
    if (is.character(x)) {
        if (anyDuplicated(x)) {
            ux <- unique(x)
            idx <- match(x, ux)
            y <- as.Date.character(ux,...)
            x <- y[idx]
        } else{
            x <- as.Date.character(x,...)
        }
    }
    return(x)
}
