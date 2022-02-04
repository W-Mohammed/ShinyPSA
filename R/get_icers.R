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
#' @param icer_data A dataframe containing \code{intervention names}, \code{qalys} & \code{costs} from which previously identified
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
    mutate(d = case_when(
      lead(costs) < costs ~ "dominated")
    ) %>%
    select(d) %>%
    pull(1)
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
    mutate(e.d = case_when(
      lead(icer) < icer ~ "e.dominated")
    ) %>%
    select(e.d) %>%
    pull(1)
}

#' Compute ICER(s)
#'
#' @param icer_data A dataframe with \code{intervention names}, \code{qalys} & \code{costs} from which previously identified
#' dominated and e.dominated interventions were removed.
#'
#' @return A matrix of \code{effects diffrentials}, \code{costs diffrentials} & \code{icers}
#' @export
#'
#' @examples
compute_ICERs <- function(icer_data) {
  icer_data %>%
    filter(if_all(c(dominated, e.dominated), ~ is.na(.))) %>%
    mutate(delta.e = c(NA, diff(qalys)),
           delta.c = c(NA, diff(costs)),
           icer = delta.c / delta.e) %>%
    select(c(delta.e, delta.c, icer)) %>%
    as.matrix()
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
    arrange(qalys) %>%
    mutate(delta.e = NA_real_,
           delta.c = NA_real_,
           dominated = NA_character_,
           e.dominated = NA_character_,
           icer = NA_real_)

  # Identify dominated interventions:
  while (any("dominated" %in%
             (icer_tmp %>%
              filter(if_any(dominated, ~ is.na(.))) %>%
              mutate(tmp = identify_dominance(icer_data = .)) %>%
              select(tmp) %>%
              .[1] %>%
              pull()))) { # check if any dominance is yet to be detected
    # do until all dominated are identified
    icer_tmp$dominated[is.na(icer_tmp$dominated)] <-
      icer_tmp[is.na(icer_tmp$dominated),] %>%
      identify_dominance()
  }

  # Compute ICER(s), before extended dominance checking:
  icer_tmp[is.na(icer_tmp$dominated) & is.na(icer_tmp$e.dominated),
           c("delta.e", "delta.c", "icer")] <- icer_tmp %>%
    compute_ICERs()

  # Identify any extendedly dominated interventions, and recompute ICER(s):
  while (any("e.dominated" %in%
             (icer_tmp %>%
              filter(if_any(icer, ~ !is.na(.))) %>%
              mutate(tmp = identify_e.dominance(icer_data = .)) %>%
              select(tmp) %>%
              .[1] %>%
              pull()))) { # check if any e.dominance is yet to be detected
    # do until all extendedly dominated are identified:
    icer_tmp$e.dominated[!is.na(icer_tmp$icer)] <-
      icer_tmp[!is.na(icer_tmp$icer),] %>%  identify_e.dominance()

    # Remove existing differentials and ICERs to avoid confusion:
    icer_tmp[, c("delta.e", "delta.c", "icer")] <- NA

    # Recompute ICER(s) for interventions that are not dominated or
    # e.dominated:
    icer_tmp[is.na(icer_tmp$dominated) & is.na(icer_tmp$e.dominated),
             c("delta.e", "delta.c", "icer")] <- icer_tmp %>%
      compute_ICERs()
  }

  return(
    icers_tab = as_tibble(icer_tmp)
  )
}

