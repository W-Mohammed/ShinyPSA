################################################################################
#
# Script Name:        icer_helper_functions.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of functions that aid icer(s)
#                     computation.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Check and add any missing columns expected by ICER computation functions
#'
#' @param .characters
#' @param .numerics
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
add_missing_columns <- function(.x, .characters, .numerics) {
  nms <- c(.characters, .numerics)
  missing_nms <- setdiff(nms, names(.x))
  .x[missing_nms[missing_nms %in% .numerics]] <- NA_real_
  .x[missing_nms[missing_nms %in% .characters]] <- NA_character_

  return(.x)
}

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

