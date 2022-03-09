################################################################################
#
# Script Name:        compute_ICERs_.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of functions that aid ICER(s)
#                     computation.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Identify dominated interventions
#'
#' @param .icer_data A table containing average costs and QALYs data
#' @param .qalys Character indicating the name of the column containing
#' Quality Adjusted Life Years (QALYs) in \code{.icer_data}
#' @param .costs Character indicating the name of the column containing
#' cost data in \code{.icer_data}
#'
#' @return A table containing \code{.icer_data} in addition to identified
#' dominance
#' @export
#'
#' @examples
identify_dominance_ <- function(.icer_data, .qalys = qalys,
                                .costs = costs) {
  # Check if missing key columns and create them if so:
  .icer_data <- .icer_data %>%
    add_missing_columns_(.x = .,
                         .characters = c("dominance", "icer_label"),
                         .numerics = c(".id", "delta.e", "delta.c",
                                       "icer"))

  # Identify dominated interventions:
  .icer_data <- .icer_data %>%
    arrange({{.qalys}}) %>%
    group_by(dominance) %>%
    mutate(
      icer_label = case_when(
        is.na(dominance) ~ case_when(
          lead({{.costs}}) < {{.costs}} ~ paste0("[dominated by ",
                                                 lead(.id), "]")),
        TRUE ~ icer_label),
      dominance = case_when(
        is.na(dominance) ~ case_when(
          lead({{.costs}}) < {{.costs}} ~ "dominated"),
        TRUE ~ dominance)) %>%
    ungroup()

  return(.icer_data)
}

#' Identify extendedly dominated interventions
#'
#' @param .icer_data A table containing average costs and QALYs data
#' @param .qalys Character indicating the name of the column containing
#' Quality Adjusted Life Years (QALYs) in \code{.icer_data}
#'
#' @return A vector stating whether any of the included interventions were
#' e.dominated
#' @export
#'
#' @examples
identify_e.dominance_ <- function(.icer_data, .qalys = qalys) {
  # Check if missing key columns and create them if so:
  .icer_data <- .icer_data %>%
    add_missing_columns_(.x = .,
                         .characters = c("dominance", "icer_label"),
                         .numerics = c(".id", "delta.e", "delta.c", "icer"))

  # Identify extendedly dominated interventions:
  .icer_data <- .icer_data %>%
    arrange({{.qalys}}) %>%
    group_by(dominance) %>%
    mutate(
      icer_label = case_when(
        is.na(dominance) ~ case_when(
          lead(icer) < icer ~ paste0("[extendedly dominated by ",
                                     lead(.id), "]")),
        TRUE ~ icer_label),
      dominance = case_when(
        is.na(dominance) ~ case_when(
          lead(icer) < icer ~ "e.dominated"),
        TRUE ~ dominance)) %>%
    ungroup()

  return(.icer_data)
}

#' Calculate ICER(s) and effects and costs differentials
#'
#' @param .icer_data A table containing average costs and QALYs data
#' @param .qalys Character indicating the name of the column containing
#' Quality Adjusted Life Years (QALYs) data in .icer_data
#' @param .costs Character indicating the name of the column containing
#' cost data in .icer_data
#'
#' @return A table of \code{effects diffrentials}, \code{costs
#' differentials} & \code{icers}
#' @export
#'
#' @examples
calculate_ICERs_ <- function(.icer_data, .qalys = qalys, .costs = costs) {
  # Check if missing key columns and create them if so:
  .icer_data <- .icer_data %>%
    add_missing_columns_(.x = .,
                         .characters = c("dominance", "icer_label"),
                         .numerics = c(".id", "delta.e", "delta.c",
                                       "icer"))

  # Compute Incremental Cost-Effectiveness Ratio (ICER):
  .icer_data <- .icer_data %>%
    arrange({{.qalys}}) %>%
    group_by(dominance) %>%
    mutate(
      delta.e = case_when(
        is.na(dominance) ~ c(NA, diff({{.qalys}}))),
      delta.c = case_when(
        is.na(dominance) ~ c(NA, diff({{.costs}}))),
      icer = case_when(
        is.na(dominance) ~ delta.c / delta.e),
      icer_label = case_when(
        is.na(dominance) & !is.na(icer) ~ paste0("[ICER = £",
                                                 round(icer, digits = 1),
                                                 ", vs ",
                                                 lag(.id), "]"),
        is.na(dominance) & is.na(icer) ~ case_when(
          n() > 1 ~ paste0("[ICER reference]"),
          TRUE ~ icer_label),
        TRUE ~ icer_label)) %>%
    ungroup()

  return(.icer_data)
}

#' Identify, iteratively, all dominated interventions
#'
#' @param .x A table containing average costs and QALYs data
#'
#' @return A dataframe with data from .x in addition to dominance
#' information, if any
#' @export
#'
#' @examples
dominance_wraper_ <- function(.x) {
  # Check if missing key columns and create them if so:
  .x <- .x %>%
    add_missing_columns_(.x = .,
                         .characters = c("dominance", "icer_label"),
                         .numerics = c(".id", "delta.e", "delta.c",
                                       "icer"))

  # Check for unidentified dominance
  while (any("dominated" %in%
             (.x %>%
              filter(if_any(dominance, ~ is.na(.))) %>%
              identify_dominance_() %>%
              pull(dominance)))) {
    # Do until all dominated are identified
    .x <- .x %>%
      identify_dominance_()
  }

  return(.x)
}

#' Identify, iteratively, all extendedly dominated interventions
#'
#' @param .x A table containing average costs and QALYs data
#'
#' @return A dataframe with data from \code{.x} in addition to extended
#' dominance information, if any
#' @export
#'
#' @examples
e.dominance_wraper_ <- function(.x) {
  # Check if missing key columns and create them if so:
  .x <- .x %>%
    add_missing_columns_(.x = .,
                         .characters = c("dominance", "icer_label"),
                         .numerics = c(".id", "delta.e", "delta.c",
                                       "icer"))

  # Check for any remaining e.dominance
  while (any("e.dominated" %in% (.x %>%
                                 filter(if_any(dominance, ~ is.na(.))) %>%
                                 identify_e.dominance_() %>%
                                 pull(dominance)))) {
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
#' @param .effs A table with Quality Adjusted Life Years (QALYs) data
#' @param .costs A table with costs data
#' @param .interventions A vector of characters with intervention names
#'
#' @return A dataframe with data from icer_data in addition to
#' \code{qalys and costs diffrential(s)}, \code{dominance} & \code{icer(s)}
#' @export
#'
#' @examples
compute_ICERs_ <- function(.icer_data, .effs = NULL, .costs = NULL,
                           .interventions = NULL) {
  # If a summary table of costs, effects and intervention names supplied:
  if(!is.null(.icer_data)) {
    # Check if missing key columns and create them if so:
    icer_tmp <- .icer_data %>%
      add_missing_columns_(.x = .,
                           .characters = c("dominance", "icer_label"),
                           .numerics = c(".id", "delta.e", "delta.c",
                                         "icer"))
  } else if(!is.null(.effs) & !is.null(.costs)) {

    # Stop if .effs & .costs are not of class tibble or have unequal dims:
    stopifnot('.effs is not a tibble' = "data.frame" %in% class(.effs),
              '.costs is not a tibble' = "data.frame" %in% class(.costs),
              '.effs and .costs have unequal dimensions' =
                dim(.effs) == dim(.costs))

    # Get number of interventions in supplied matrix:
    n.comparators <- ncol(.effs)

    # Check supplied interventions labels, create ones if any is missing:
    if(!is.null(.interventions) &
       length(.interventions) != n.comparators) {
      .interventions <- NULL
    }
    if(is.null(.interventions)) {
      .interventions <- paste("intervention", 1:n.comparators)
    }

    # Define ICER table:
    icer_tmp <- tibble(
      'intervention' = .interventions,
      'qalys' = colMeans(.effs),
      'costs' = colMeans(.costs)) %>%
      add_missing_columns_(.x = .,
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

  # Drop .id after combining it with intervention's name:
  icer_tmp <- icer_tmp %>%
    mutate(intervention = paste0(.id, ": ", intervention)) %>%
    select(-.id)

  return(icer_tmp)
}

