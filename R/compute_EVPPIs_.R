################################################################################
#
# Script Name:        compute_EVPPIs_.R
# Module Name:        Economic/PSA
# Script Description: Defines EVPPIs estimating function.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Compute Expected Value of Perfect Partial Information (EVPPI)
#'
#' @param .PSA_data A list of class shinyPSA that contains summary PSA
#' results.
#' @param .effs A matrix containing the \code{effects} from PSA. Number of
#' \code{columns} is equal to the interventions while the number of
#' \code{rows} is equal to the number of PSA simulations to be summarised.
#' @param .costs A matrix containing the \code{costs} from PSA. Number of
#' \code{columns} is equal to the interventions while the number of
#' \code{rows} is equal to the number of PSA simulations to be summarised.
#' @param .params A matrix containing parameters' configurations used in
#' the PSA.
#' @param .MAICER_ The Maximum acceptable incremental cost-effectiveness ratio.
#' (MAICER) to be considered in the summary table. Default value is
#' \code{30,000}.
#' @param .units_ A character, the units to associate with the
#' monitory values in the summary table. Default is sterling pounds
#' (GBP) \code{"\u00A3"}.
#' @param .individual_evppi_ Logical (default \code{TRUE}) to return per person
#' EVPPI, otherwise population EVPPI will be reported.
#' @param .evppi_population_ The size of the population that is annually
#' affected by the competing health technologies under evaluation.
#' @param .discount_rate_ The discount rate used to discount future affected
#' populations.
#' @param .time_horion_ The time expected to pass (in years) before the
#' interventions under consideration change (how long before the decision
#' under consideration become obsolete or requires updating).
#' @param .session
#'
#' @return A list containing the EVPPI results table and caption information.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ShinyPSA)
#'
#' EVPPI_ind_res <- compute_EVPPIs_(
#'     .PSA_data = PSA_data,
#'     .MAICER_ = 30000)
#'
#' EVPPI_pop_res <- compute_EVPPIs_(
#'     .PSA_data = PSA_data,
#'     .MAICER_ = 20000,
#'     .individual_evppi_ = FALSE,
#'     .evppi_population_ = 1000,
#'     .time_horion_ = 5)
#' }
compute_EVPPIs_ <- function(.PSA_data,
                            .effs = NULL,
                            .costs = NULL,
                            .params = NULL,
                            .MAICER_ = 30000,
                            .units_ = "\u00A3",
                            .individual_evppi_ = TRUE,
                            .evppi_population_ = NULL,
                            .discount_rate_ = 0.035,
                            .time_horion_ = NULL,
                            .session = NULL) {

  # Sanity checks:----
  if(is.null(.effs) | is.null(.costs) | is.null(.params)) {
    .effs = .PSA_data$e
    .costs = .PSA_data$c
    .params = .PSA_data$p
  }

  # Estimate individual EVPPI:----
  ## Calculate incremental net benefit (INB):----
  inb <- createInb( # Strong et al. function
    costs.int = .costs,
    effects.int = .effs,
    lambda = .MAICER_)

  EVPPI <- applyCalcSingleParamGam( # Strong et al. function
    .params = .PSA_data$p,
    nb = inb,
    session = .session)

  # Estimate population EVPPI if user provided necessary data:----
  discounted_population = 1
  table_caption = "Individual EVPPI"
  if(!.individual_evppi_) {
    if(is.null(.evppi_population_) | is.null(.time_horion_)) {
      .individual_evppi_ <- TRUE
      message("Population EVPPI or decision time horizon were not supplied.
              The function will calculate individual EVPPI")
    }
  }
  if(!.individual_evppi_) {
    ## Re-estimate discounted population for population EVPPI:----
    discounted_population <- sum(
      .evppi_population_ / ((1 + .discount_rate_)^(1:.time_horion_)))
    table_caption = paste0("Population EVPPI:- ",
                           "Population size: ", .evppi_population_, "; ",
                           "Time horizon: ", .time_horion_, " year(s); ",
                           "Discount rate: ", .discount_rate_ * 100, "%.")
  }

  # Prepare EVPI:----
  EVPI <- if(!is.null(.PSA_data[["EVPI"]])) {
    ## Get EVPI data from the PSA_data object:----
    dplyr::tibble(
      'EVPI_values' = .PSA_data[["EVPI"]] * discounted_population,
      ## put WTP in a column next to EVPI:----
      'WTP_values' = .PSA_data[["WTPs"]]) %>%
      ## filter and keep values corresponding to ones the in .MAICER_ vector:----
    dplyr::filter(WTP_values %in% .MAICER_) %>%
      ## rename WTP values to use as column names later:----
    dplyr::pull(var = EVPI_values)
  } else {
    ## Calculate EVPI from inputs if PSA_data object was not provided:----
    ShinyPSA::compute_EVPIs_(
      .effs = .effs,
      .costs = .costs)
  }

  # Prepare EVPPI results table:----
  ## Build the individual EVPPI results table:----
  tmp_name <- paste0("Per Person EVPPI (", .units_, "). MAICER = ",
                     scales::dollar(
                       x = .MAICER_,
                       prefix = .units_))
  ind_evppi <- dplyr::tibble(
    "Parameters" =
      colnames(.PSA_data$p),
    {{tmp_name}} :=
      round(EVPPI[, 1], 2),
    "Standard Error" =
      round(EVPPI[, 2], 2),
    "Indexed to Overall EVPI (%)" =
      scales::percent(round((EVPPI[, 1] / EVPI), 2)))
  ## Build the population EVPPI results table:----
  pop_evppi <- NULL
  if(!isTRUE(.individual_evppi_)) {
    tmp_name <- paste0("Population EVPPI over ", .time_horion_,
                       " years (", .units_, ")")
    pop_evppi <- ind_evppi %>%
      dplyr::mutate(
        {{tmp_name}} :=
          signif(EVPPI[, 1] * discounted_population, 4)
      )
  }

  if(!is.null(pop_evppi)) {
    return(
      list('Population EVPPI' = pop_evppi,
           'Caption' = table_caption))
  } else {
    return(
      list('Population EVPPI' = ind_evppi,
           'Caption' = table_caption))
  }
}

