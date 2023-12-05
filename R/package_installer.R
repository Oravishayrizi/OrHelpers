#' Fast and compact way to install/load libraries
#'
#' This function take a vector of packages names as input and: install any package that is currently not installed.
#' Then, it loads all the packages.
#'
#' @param packages a vector of packages names.
#' @return The value that appeared most in a set of data values
#' @details The function install packages from CRAN repository. Other repositories including Github does not supported.
#' @references Based on <https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/>.
#' @export
package_installer<-function(packages){
    not_installed <- !packages %in% installed.packages()
    if (any(not_installed)) install.packages(packages[not_installed])
    invisible(lapply(packages,require,character.only=TRUE))
}
