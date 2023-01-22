#' Fast change to date class
#'
#' This function is a wrapper function of the `lubridate::fast_strptime` function. It coverts a character or factor vector into a vector of dates.
#'
#' @param x a vector to be converted into date.
#' @param format the format of the dates. The default format is "2020-12-30", different format can be used but should be explicitly defined. For other formats of dates look at: <https://www.stat.berkeley.edu/~s133/dates.html>
#' @return A vector of Dates in format of `YYYY-MM-DD`
#' @export
#' @seealso see [`lubridate::fast_strptime`] help for more information
#' @examples
#' fast_as_date(c("2020-10-15","2020-12-03"))
#' fast_as_date(c("10/15/20","12/03/20"),format='%m/%d/%y')
#' fast_as_date(c("October 15, 2020","December 03, 2020"),format='%B %d, %Y')
fast_as_date<-function(x,format=NULL){
    if (is.null(format)) {
        format<-"%Y-%m-%d"
    }
    if(!is.vector(x)){
        stop('x should be a vector')
    }

    if (is.factor(x)) {
        x<-as.character(x)
    }
    if (is.character(x))
        x<-as.Date(lubridate::fast_strptime(x,format))
    return(x)
}
