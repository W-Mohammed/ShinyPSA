################################################################################
#
# Script Name:        helper_functions.R
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
add_missing_columns_ <- function(.x, .characters, .numerics) {
  # Check for missing columns:
  missing_nms <- setdiff(c(.numerics, .characters), names(.x))

  # In case there were missing columns:
  if(!length(missing_nms) == 0) {
    # Create missing columns:
    .x <- .x %>%
      cbind(missing_nms %>%
              `names<-`(missing_nms) %>%
              map_dfc(.f = function(.x) {
                .x = ifelse(.x %in% .numerics, NA_real_, NA_character_)
              }))
  }
  .x <- .x %>%
    select(".id", everything()) %>%
    mutate(.id = row_number())

  return(.x)
}

#' Calculate differential costs and QALYs
#'
#' @param .data A dataframe containing costs or QALYs data for which the
#' function is to estimate differential values
#' @param .ref An integer indicating the index of the reference intervention
#'
#' @return
#' @export
#'
#' @examples
calculate_differentials_ <- function(.data, .ref) {
  ref_data <- .data %>%
    pull({{.ref}})
  differentials_data <- .data %>%
    mutate(across(.fns = function(.x) {
      .x - ref_data
    }))

  return(differentials_data)
}

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

