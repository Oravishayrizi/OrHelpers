#' Rounding of Numbers
#'
#' This function rounds the values in its first argument to the specified number of decimal places (default 0).
#' un like the native round function: .5 is rounded up.
#'
#' @param x a numeric vector
#' @param digits integer indicating the number of decimal places (round), negative values are allowed (see `Details`)
#' @details Rounding to a negative number of digits means rounding to a power of ten, so for example round(x, digits = -2) rounds to the nearest hundred.
#' @return The number of unique values in the vector
#' @export
#' @seealso See [`base::round`] help for more information related the "round to even" method.
#' @examples
#' round(c(1.5,2.5),digits=0) #base R round function - "round to even"
#' round2(c(1.5,2.5),digits=0) #Unlike the base R function - round 0.5 up
#' round2(c(254,255,256),-1) #round to nearest ten
#' round2(c(254,255,256),-2) #round to nearest hundred
#' round2(c(2.54,2.55,2.56),1)
round2<- function(x,digits=0) {
    dig<-10^digits
    floor(0.5+x*dig)/dig
}

