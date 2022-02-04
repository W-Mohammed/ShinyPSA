#' Get the maximum value in all rows in a dataframe.
#'
#' @param x A matrix or dataframe with \code{r} rows and \code{c} columns
#'
#' @return A dataframe with \code{r} rows and \code{1} column
#' @export
#'
#' @examples
#'
#'
#'
get_row_max <- function(x){
  do.call(pmax, as.data.frame(x))
}
