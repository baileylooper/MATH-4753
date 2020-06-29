#' @title findmode
#'
#' @description this function returns the mode of a given data set, which is the value in the data set that occurs the most amount of times
#'
#' @param x A numerical vector
#'
#' @return the mode of the given data set
#' @export
#'
#' @examples
#' x=c(1, 2, 3, 3, 3, 5, 6, 7, 7, 8, 9, 11, 12, 13, 13, 13, 13, 17, 20)
#' mode(x)
#' result = 13
findmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
