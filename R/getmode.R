#' Find the statistical mode
#'
#' This function finds the statistical mode (the value that appears most often). In case of two modes, the first value will be return.
#'
#' @param x a vector.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds. The default is FALSE.
#' @return The value that appeared most in a set of data values
#' @references Based on <https://stackoverflow.com/a/8189441>
#' @export
#' @examples
#' getmode(c(1,1,2,3,NA,NA,NA))
#' getmode(c(1,1,2,3,NA,NA,NA),na.rm=TRUE)
#' getmode(c("A","B","C","A",NA,NA,NA),na.rm=TRUE)
getmode <- function(x,na.rm=FALSE) {
    if (na.rm==TRUE) {
        x<-x[!is.na(x)]
    }
    uniqv <- unique(x)
    uniqv[which.max(tabulate(match(x, uniqv)))]
}

