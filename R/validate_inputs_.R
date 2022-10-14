###########################################################################
#
# Script Name:        validate_inputs.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of functions that aid validation of
#                     ShinyPSA inputs.
#                     computation.
# Author:             WM-University of Sheffield
#                     email: (wmamohammed1@sheffield.ac.uk)
#
###########################################################################

#' Check PSA data structures passed to a shiny app
#'
#' @param .costs_ A tibble/dataframe/matrix containing costs data.
#' @param .effs_ A tibble/dataframe/matrix containing effects data.
#' @param .params_ A tibble/dataframe/matrix containing parameters data.
#' @param .id_ A letter "c", "e" or "p" that declares if the function is
#' to check costs, effects or parameters data structures.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
check_PSA_inputs <- function(.costs_, .effs_, .params_, .id_) {
  # read inputs:
  ## costs:
  if(shiny::isTruthy(.costs_))
    .costs_ <- readr::read_csv(
      file = .costs_$datapath,
      show_col_types = FALSE
    )
  ## effects:
  if(shiny::isTruthy(.effs_))
    .effs_ <- readr::read_csv(
      file = .effs_$datapath,
      show_col_types = FALSE
    )
  ## parameters:
  if(shiny::isTruthy(.params_))
    .params_ <- readr::read_csv(
      file = .params_$datapath,
      show_col_types = FALSE
    )

  switch (.id_,
          c = if(shiny::isTruthy(.effs_)) {
            if(all(dim(.costs_) == dim(.effs_))) {
              if(shiny::isTruthy(.params_)) {
                if(nrow(.costs_) == nrow(.params_)) {

                  return(
                    NULL
                  )
                } else {

                  return(
                    "Costs and Effects data have equal number of rows and columns,
            but Costs and Parameters data have different number of rows!"
                  )
                }
              } else {

                return(
                  NULL
                )
              }
            } else {

              return(
                "Costs and Effects data have different number of columns or rows!"
              )
            }
          } else {
            if(shiny::isTruthy(.params_)) {
              if(nrow(.costs_) == nrow(.params_)) {

                return(
                  NULL
                )
              } else {

                return(
                  "Costs and Parameters data have different number of rows!"
                )
              }
            } else {

              return(
                NULL
              )
            }
          },
          e = if(shiny::isTruthy(.costs_)) {
            if(all(dim(.effs_) == dim(.costs_))) {
              if(shiny::isTruthy(.params_)) {
                if(nrow(.effs_) == nrow(.params_)) {

                  return(
                    NULL
                  )
                } else {

                  return(
                    "Effects and Costs data have equal number of rows and columns,
            but Effects and Parameters data have different number of rows!"
                  )
                }
              } else {

                return(
                  NULL
                )
              }
            } else {

              return(
                "Effects and Costs data have different number of columns or rows!"
              )
            }
          } else {
            if(shiny::isTruthy(.params_)) {
              if(nrow(.effs_) == nrow(.params_)) {

                return(
                  NULL
                )
              } else {

                return(
                  "Effects and Parameters data have different number of rows!"
                )
              }
            } else {

              return(
                NULL
              )
            }
          },
          p = if(shiny::isTruthy(.costs_)) {
            if(nrow(.costs_) == nrow(.params_)) {
              if(shiny::isTruthy(.effs_)) {
                if(nrow(.effs_) == nrow(.params_)) {

                  return(
                    NULL
                  )
                } else {

                  return(
                    "Parameters and Costs data have equal number of rows,
            but Parameters and Effects data have different number of rows!"
                  )
                }
              } else {

                return(
                  NULL
                )
              }
            } else {

              return(
                "Parameters and Costs data have different number of rows!"
              )
            }
          } else {
            if(shiny::isTruthy(.effs_)) {
              if(nrow(.params_) == nrow(.effs_)) {

                return(
                  NULL
                )
              } else {

                return(
                  "Parameters and Effects data have different number of rows!"
                )
              }
            } else {

              return(
                NULL
              )
            }
          },
  )
}

#' Check if data structure columns are all numerics.
#'
#' @param .data_ The data structure to check if all are numerics.
#' @param .label_ The name of the data structure being checked.
#'
#' @return A logical or message based on whether non-numeric columns exists.
#' @export
#'
#' @examples
#' \dontrun{
#' }
check_numerics_ <- function(.data_, .label_) {
  .data_ <- readr::read_csv(
    file = .data_$datapath,
    show_col_types = FALSE)

  tmp <- purrr::map_lgl(
    .x = .data_,
    .f = function(.col_) {
      is.numeric(.col_)
    })

  return(
    if(all(tmp)) {
      NULL
    } else {
      glue::glue("{.label_} data has one or more non-numeric columns!")
    }
  )
}

#' Check if data structure contain missing (NA) values.
#'
#' @param .data_ The data structure to check for missing values.
#' @param .label_ The name of the data structure being checked.
#'
#' @return A logical or message based on whether missing data exists.
#' @export
#'
#' @examples
#' \dontrun{
#' }
check_missings_ <- function(.data_, .label_) {
  .data_ <- readr::read_csv(
    file = .data_$datapath,
    show_col_types = FALSE)

  tmp <- any(
    is.na(.data_)
  )

  return(
    if(!tmp) {
      NULL
    } else {
      glue::glue("{.label_} data has one or more missing values!")
    }
  )
}
