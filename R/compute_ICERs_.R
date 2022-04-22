################################################################################
#
# Script Name:        compute_ICERs_.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of functions that aid ICER(s)
#                     computation.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

# Identify dominated interventions
#
# @param .icer_data A table containing average costs and QALYs data
# @param .qalys Character indicating the name of the column containing
# Quality Adjusted Life Years (QALYs) in \code{.icer_data}
# @param .costs Character indicating the name of the column containing
# cost data in \code{.icer_data}
#
# @return A table containing \code{.icer_data} in addition to identified
# dominance
#
# @examples
# \dontrun{}
identify_dominance_ <- function(.icer_data, .qalys = qalys,
                                .costs = costs) {
  # Check if missing key columns and create them if so:
  .icer_data <- .icer_data %>%
    add_missing_columns_(
      .x = .,
      .characters = c("dominance", "icer_label"),
      .numerics = c(".id", "delta.e", "delta.c", "icer"))

  # Identify dominated interventions:
  .icer_data <- .icer_data %>%
    dplyr::arrange({{.qalys}}) %>%
    dplyr::group_by(dominance) %>%
    dplyr::mutate(
      icer_label = dplyr::case_when(
        is.na(dominance) ~ dplyr::case_when(
          dplyr::lead({{.costs}}) < {{.costs}} ~ "SD"),
        TRUE ~ icer_label),
      dominance = dplyr::case_when(
        is.na(dominance) ~ dplyr::case_when(
          dplyr::lead({{.costs}}) < {{.costs}} ~ "SD"),
        TRUE ~ dominance)) %>%
    dplyr::ungroup()

  return(.icer_data)
}

# Identify extendedly dominated interventions
#
# @param .icer_data A table containing average costs and QALYs data
# @param .qalys Character indicating the name of the column containing
# Quality Adjusted Life Years (QALYs) in \code{.icer_data}
#
# @return A vector stating whether any of the included interventions were
# e.dominated
#
# @examples
# \dontrun{}
identify_e.dominance_ <- function(.icer_data, .qalys = qalys) {
  # Check if missing key columns and create them if so:
  .icer_data <- .icer_data %>%
    add_missing_columns_(
      .x = .,
      .characters = c("dominance", "icer_label"),
      .numerics = c(".id", "delta.e", "delta.c", "icer"))

  # Identify extendedly dominated interventions:
  .icer_data <- .icer_data %>%
    dplyr::arrange({{.qalys}}) %>%
    dplyr::group_by(dominance) %>%
    dplyr::mutate(
      icer_label = dplyr::case_when(
        is.na(dominance) ~ dplyr::case_when(
          dplyr::lead(icer) < icer ~ "ED"),
        TRUE ~ icer_label),
      dominance = dplyr::case_when(
        is.na(dominance) ~ dplyr::case_when(
          dplyr::lead(icer) < icer ~ "ED"),
        TRUE ~ dominance)) %>%
    dplyr::ungroup()

  return(.icer_data)
}

# Calculate ICER(s) and effects and costs differentials
#
# @param .icer_data A table containing average costs and QALYs data
# @param .qalys Character indicating the name of the column containing
# Quality Adjusted Life Years (QALYs) data in .icer_data
# @param .costs Character indicating the name of the column containing
# cost data in .icer_data
#
# @return A table of \code{effects diffrentials}, \code{costs
# differentials} & \code{icers}
#
# @examples
# \dontrun{}
calculate_ICERs_ <- function(.icer_data, .qalys = qalys, .costs = costs) {
  # Check if missing key columns and create them if so:
  .icer_data <- .icer_data %>%
    add_missing_columns_(
      .x = .,
      .characters = c("dominance", "icer_label"),
      .numerics = c(".id", "delta.e", "delta.c", "icer"))

  # Compute Incremental Cost-Effectiveness Ratio (ICER):
  .icer_data <- .icer_data %>%
    dplyr::arrange({{.qalys}}) %>%
    dplyr::group_by(dominance) %>%
    dplyr::mutate(
      delta.e = dplyr::case_when(
        is.na(dominance) ~ c(NA, diff({{.qalys}}))),
      delta.c = dplyr::case_when(
        is.na(dominance) ~ c(NA, diff({{.costs}}))),
      icer = dplyr::case_when(
        is.na(dominance) ~ delta.c / delta.e),
      icer_label = dplyr::case_when(
        is.na(dominance) & !is.na(icer) ~ paste0("ICER = ",
                                                 scales::dollar(
                                                   x = icer,
                                                   prefix = "\u00A3")),
        is.na(dominance) & is.na(icer) ~ dplyr::case_when(
          dplyr::n() > 1 ~ paste0("reference"),
          TRUE ~ icer_label),
        TRUE ~ icer_label)) %>%
    dplyr::ungroup()

  return(.icer_data)
}

# Identify, iteratively, all dominated interventions
#
# @param .x A table containing average costs and QALYs data
#
# @return A dataframe with data from .x in addition to dominance
# information, if any
#
# @examples
# \dontrun{}
dominance_wraper_ <- function(.x) {
  # Check if missing key columns and create them if so:
  .x <- .x %>%
    add_missing_columns_(
      .x = .,
      .characters = c("dominance", "icer_label"),
      .numerics = c(".id", "delta.e", "delta.c", "icer"))

  # Check for unidentified dominance
  while (any("SD" %in%
             (.x %>%
              dplyr::filter(dplyr::if_any(dominance, ~ is.na(.))) %>%
              identify_dominance_() %>%
              dplyr::pull(dominance)))) {
    # Do until all dominated are identified
    .x <- .x %>%
      identify_dominance_()
  }

  return(.x)
}

# Identify, iteratively, all extendedly dominated interventions
#
# @param .x A table containing average costs and QALYs data
#
# @return A dataframe with data from \code{.x} in addition to extended
# dominance information, if any
#
# @examples
# \dontrun{}
e.dominance_wraper_ <- function(.x) {
  # Check if missing key columns and create them if so:
  .x <- .x %>%
    add_missing_columns_(
      .x = .,
      .characters = c("dominance", "icer_label"),
      .numerics = c(".id", "delta.e", "delta.c", "icer"))

  # Check for any remaining e.dominance
  while (any("ED" %in%
             (.x %>%
              dplyr::filter(dplyr::if_any(dominance, ~ is.na(.))) %>%
              identify_e.dominance_() %>%
              dplyr::pull(dominance)))) {
    # Do until all extendedly dominated are identified:
    .x <- .x %>%
      identify_e.dominance_() %>%
      calculate_ICERs_() # ICER(s) for un-dominated/e.dominated
  }

  return(.x)
}

#' Compute ICER(s)
#'
#' @param .icer_data A table containing average costs and QALYs data
#' @param .effs A tibble containing the \code{effects} from PSA. Number of
#'  \code{columns} is equal to the interventions while the number of
#'  \code{rows} is equal to the number of PSA simulations to be summarised.
#' @param .costs A tibble containing the \code{costs} from PSA. Number of
#'  \code{columns} is equal to the interventions while the number of
#'  \code{rows} is equal to the number of PSA simulations to be summarised.
#' @param .interventions A vector containing the names of all
#' interventions. If not provided or less names than needed is provided,
#' the function will generate generic names, for example
#' \code{intervention 1}.
#'
#' @return A dataframe with data from icer_data in addition to
#' \code{qalys and costs diffrential(s)}, \code{dominance} & \code{icer(s)}
#' @export
#'
#' @examples
#' \dontrun{}
compute_ICERs_ <- function(.icer_data, .effs = NULL, .costs = NULL,
                           .interventions = NULL) {
  # If a summary table of costs, effects and intervention names supplied:
  if(!is.null(.icer_data)) {
    # Check if missing key columns and create them if so:
    icer_tmp <- .icer_data %>%
      add_missing_columns_(
        .x = .,
        .characters = c("dominance", "icer_label"),
        .numerics = c(".id", "delta.e", "delta.c", "icer"))
  } else if(!is.null(.effs) & !is.null(.costs)) {

    # Stop if .effs & .costs are not of class tibble or have unequal dims:
    stopifnot('.effs is not a tibble' = "data.frame" %in% class(.effs),
              '.costs is not a tibble' = "data.frame" %in% class(.costs),
              '.effs and .costs have unequal dimensions' =
                dim(.effs) == dim(.costs))

    # Get number of interventions in supplied matrix:
    n.comparators <- ncol(.effs) # Number of interventions

    # Check supplied interventions labels, create ones if any is missing:
    if(!is.null(.interventions) & length(.interventions) != n.comparators) {
      .interventions <- NULL
    }
    if(is.null(.interventions)) {
      .interventions <- paste("intervention", 1:n.comparators)
    }

    # Define ICER table:
    icer_tmp <- dplyr::tibble(
      'intervention' = .interventions,
      'qalys' = colMeans(.effs),
      'costs' = colMeans(.costs)) %>%
      add_missing_columns_(
        .x = .,
        .characters = c("dominance", "icer_label"),
        .numerics = c(".id", "delta.e", "delta.c",
                      "icer"))
  } else {
    stop("Please supply costs and effects from PSA, each in a separate
         tibble/dataframe, or a summary table with interventions' names,
         and corresponding mean costs and mean qalys")
  }

  # Identify dominated interventions:
  icer_tmp <- icer_tmp %>%
    dominance_wraper_()

  # Compute ICER(s), before extended dominance checking:
  icer_tmp <- icer_tmp %>%
    calculate_ICERs_()

  # Identify any extendedly dominated interventions, and recompute ICER(s):
  icer_tmp <- icer_tmp %>%
    e.dominance_wraper_()

  # Drop .id:
  icer_tmp <- icer_tmp %>%
    select(-.id)

  return(icer_tmp)
}

