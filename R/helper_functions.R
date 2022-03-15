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
#' @param .data_ A dataframe containing costs or QALYs data for which the
#' function is to estimate differential values
#' @param .ref An integer indicating the index of the reference intervention
#'
#' @return
#' @export
#'
#' @examples
calculate_differentials_ <- function(.data_, .ref) {
  differentials_data <- .data_ %>%
    mutate(across(.fns = function(.x) {
      .x - .data_ %>%
        pull({{.ref}})
    }))

  return(differentials_data)
}

#' Assign extra arguments/parameters in parent function
#'
#' @param .default_args_ # A list containing default arguments names and
#' their values.
#' @param .env_ # Environment object grabbed from the parent function's
#' environment to correctly assign arguments to that function.
#' @param .args_ # A list containing supplied/additional arguments names
#' and their values. Arguments in .default_args_ but existing in .args_
#' will be assigned values from .args_ and vice versa.
#'
#' @return This function assigns variables/objects in the parent's function
#' environment, hence it returns nothing.
#' @export
#'
#' @examples
assign_extraArgs_ <- function(.default_args_, .env_, .args_) {
  # Grab default arguments' names:
  if(is.null(names(.default_args_)))
    stop(".default_args_ should contain named objects")
  if(length(names(.default_args_)) != length(.default_args_))
    stop("all arguments in .default_args_ should be named")
  expected_args_names <- names(.default_args_)
  # Grab additional arguments' names:
  supplied_args_names <- names(.args_)
  # Let the user know if any of the supplied arguments were unrecognised:
  if(any(!supplied_args_names %in% expected_args_names))
    message("Argument(s) ",
            paste(supplied_args_names[!supplied_args_names %in%
                                        expected_args_names]),
            " is/are unknown, and therefore ignored")
  # Set additional arguments:
  purrr::walk(
    .x = expected_args_names,
    .f = function(.arg) {
      assign(.arg,
             if(is.null(.args_[[.arg]])) {
               .default_args_[[.arg]]
             } else {
               .args_[[.arg]]
             }, envir = .env_)
    })
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

