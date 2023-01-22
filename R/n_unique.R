#' Count the number of unique values
#'
#' This function counts the number of unique values.
#'
#' @param x a vector of dates/factor levels/number
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds. The default is FALSE.
#' @return The number of unique values in the vector
#' @export
#' @examples
#' n_unique(c(1,1,2,3,NA,NA,NA))
#' n_unique(c(1,1,2,3,NA,NA,NA),na.rm=TRUE)
#' n_unique(c("A","B","C","A",NA,NA,NA))
#' n_unique(c("A","B","C","A",NA,NA,NA),na.rm=TRUE)
n_unique<-function(x,na.rm=FALSE) {
    if (na.rm==TRUE) {
        x<-x[!is.na(x)]
    }
    num_of_unique_values<-length(unique(as.character(x)))
    return(num_of_unique_values)
}
