################################################################################
#
# Script Name:        helper_functions.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of functions that aid icer(s)
#                     computation.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

# Check and add any missing columns expected by ICER computation functions
#
# @param .x Dataframe/tibble for which the function will ensure existence
# of, and if required creation of, columns with given names in
# \code{.characters} and \code{.numerics}
# @param .characters A vector of character strings defining the names of
# of columns of type \code{character} that the function would check if
# exist in the dataframe, and creating the ones that do not exist.
# @param .numerics A vector of character strings defining the names of
# of columns of type \code{numeric} that the function would check if
# exist in the dataframe, and creating the ones that do not exist.
#
# @return A tibble with all columns from tibble \code{.x} in addition to
# any columns with names in \code{.characters} and \code{.numerics}.
#
# @examples
# \donotrun{}
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

# Calculate differential costs and QALYs
#
# @param .data_ A dataframe containing costs or QALYs data for which the
# function is to estimate differential values
# @param .ref An integer indicating the index of the reference intervention
#
# @return A tibble with the same dimensions of \code{.data_}, but with
# differential values based on the data in column \code{.ref}.
#
# @examples
# \donotrun{}
calculate_differentials_ <- function(.data_, .ref) {
  differentials_data <- .data_ %>%
    mutate(across(.fns = function(.x) {
      .x - .data_ %>%
        pull({{.ref}})
    }))

  return(differentials_data)
}

# Assign extra arguments/parameters in parent function
#
# @param .default_args_ A list containing default arguments names and
# their values.
# @param .env_ Environment object grabbed from the parent function's
# environment to correctly assign arguments to that function.
# @param .args_ A list containing supplied/additional arguments names
# and their values. Arguments in .default_args_ but existing in .args_
# will be assigned values from .args_ and vice versa.
#
# @return This function assigns variables/objects in the parent's function
# environment, hence it returns nothing.
#
# @examples
# \donotrun{}
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
    message("Argument(s) [",
            paste(supplied_args_names[!supplied_args_names %in%
                                        expected_args_names]),
            "] is/are unknown, and therefore ignored")
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
