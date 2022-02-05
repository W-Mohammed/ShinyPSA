################################################################################
#
# Script Name:        get_icers.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of functions that aid icer(s)
#                     computation.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Identify dominated interventions
#'
#' @param icer_data A dataframe containing \code{intervention names},
#'  \code{qalys} & \code{costs} from which previously identified
#' dominated interventions were removed.
#'
#' @return  A vector stating whether any of the included interventions were
#' dominated.
#' @export
#'
#' @examples
identify_dominance <- function(icer_data) {
  icer_data %>%
    arrange(qalys) %>%
    group_by(dominance) %>%
    mutate(
      icer_label = case_when(
        is.na(dominance) ~ case_when(
          lead(costs) < costs ~ paste("dominated by", lead(intervention))),
        TRUE ~ icer_label),
      dominance = case_when(
        is.na(dominance) ~ case_when(
          lead(costs) < costs ~ "dominated"),
        TRUE ~ dominance)) %>%
    ungroup()
}

#' Identify extendedly dominated interventions
#'
#' @param icer_data A dataframe containing \code{intervention names},
#' \code{qalys} & \code{costs} from which previously identified
#' dominated and e.dominated interventions were removed.
#'
#' @return A vector stating whether any of the included interventions were
#' e.dominated
#' @export
#'
#' @examples
identify_e.dominance <- function(icer_data) {
  icer_data %>%
    arrange(qalys) %>%
    group_by(dominance) %>%
    mutate(
      icer_label = case_when(
        is.na(dominance) ~ case_when(
          lead(icer) < icer ~ paste("e.dominated by", lead(intervention))),
        TRUE ~ icer_label),
      dominance = case_when(
        is.na(dominance) ~ case_when(
          lead(icer) < icer ~ "e.dominated"),
        TRUE ~ dominance)) %>%
    ungroup()
}

#' Compute ICER(s)
#'
#' @param icer_data A dataframe with \code{intervention names},
#'  \code{qalys} & \code{costs} from which previously identified
#' dominated and e.dominated interventions were removed.
#'
#' @return A matrix of \code{effects diffrentials}, \code{costs
#'  diffrentials} & \code{icers}
#' @export
#'
#' @examples
compute_ICERs <- function(icer_data) {
  icer_data %>%
    group_by(dominance) %>%
    mutate(
      delta.e = case_when(
        is.na(dominance) ~ c(NA, diff(qalys))),
      delta.c = case_when(
        is.na(dominance) ~ c(NA, diff(costs))),
      icer = case_when(
        is.na(dominance) ~ delta.c / delta.e),
      icer_label = case_when(
        is.na(dominance) & !is.na(icer) ~ paste("ICER vs", lag(intervention)),
        TRUE ~ icer_label)) %>%
    ungroup()
}

#' Get ICERs and effects and costs differentials
#'
#' @param icer_data A dataframe containing \code{intervention names}, \code{qalys} & \code{costs}
#'
#' @return A dataframe with data from icer_data in addition to
#' \code{qalys and costs diffrential(s)} & \code{icer(s)}
#' @export
#'
#' @examples
get_icers <- function(icer_data) {
  # Sort ICER table and empty columns:
  icer_tmp <- icer_data %>%
    mutate(delta.e = NA_real_,
           delta.c = NA_real_,
           dominance = NA_character_,
           icer = NA_real_,
           icer_label = NA_character_)

  # Identify dominated interventions:
  while (any("dominated" %in% (icer_tmp %>%
                               filter(if_any(dominance, ~ is.na(.))) %>%
                               identify_dominance(icer_data = .) %>%
                               pull(dominance)))) { # check for unidentified dominance
    # do until all dominated are identified
    icer_tmp <- icer_tmp %>%
      identify_dominance()
  }

  # Compute ICER(s), before extended dominance checking:
  icer_tmp <- icer_tmp %>%
    compute_ICERs()

  # Identify any extendedly dominated interventions, and recompute ICER(s):
  while (any("e.dominated" %in% (icer_tmp %>%
                               filter(if_any(dominance, ~ is.na(.))) %>%
                               identify_e.dominance(icer_data = .) %>%
                               pull(dominance)))) { # check if any
    # e.dominance is yet to be detected
    # do until all extendedly dominated are identified:
    icer_tmp <- icer_tmp %>%
      identify_e.dominance()

    # Recompute ICER(s) for interventions that are not dominated or
    # e.dominated:
    icer_tmp <- icer_tmp %>%
      compute_ICERs()
  }

  return(
    icers_tab = as_tibble(icer_tmp)
  )
}

