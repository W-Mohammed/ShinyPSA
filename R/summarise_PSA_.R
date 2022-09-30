################################################################################
#
# Script Name:        summarise_PSA.R
# Module Name:        Economic/PSA
# Script Description: Defines the function that summarises probabilistic
#                     sensitivity analysis outputs.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Summarise PSA outputs and report results
#'
#' @param .effs A matrix containing the \code{effects} from PSA. Number of
#'  \code{columns} is equal to the interventions while the number of
#'  \code{rows} is equal to the number of PSA simulations to be summarised.
#' @param .costs A matrix containing the \code{costs} from PSA. Number of
#'  \code{columns} is equal to the interventions while the number of
#'  \code{rows} is equal to the number of PSA simulations to be summarised.
#' @param .params A matrix containing parameters' configurations used in
#' the PSA.
#' @param .interventions A vector containing the names of all
#' interventions. If not provided or less names than needed is provided,
#' the function will generate generic names, for example
#' \code{intervention 1}.
#' @param .ref An integer indicating the index of the reference
#' intervention. This parameter is ignored if more than two
#' \code{interventions} are under analysis.
#' @param .Kmax The maximum willingness-to-pay threshold to use in the
#' analysis. This parameter is ignored if \code{wtp} is provided.
#' @param .wtp A vector of numerical values declaring the
#' willingness-to-pay (WTP) values to use in the analysis. If \code{NULL}
#' (default) a range of WTP values (up to \code{.Kmax} will be used.
#' @param .max_Kpoints Maximum number of willingness-to-pay values (default
#' 100) to use in the analysis.
#' @param .lambda Maximum acceptable ICER, default is 30,000.
#' @param .plot A boolean, FALSE (default), for whether to generate plots.
#'
#' @return A list of class \code{psa} containing several objects.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ShinyPSA)
#'
#' PSA_summary = summarise_PSA_(
#'   .effs = ShinyPSA::Brennan_1K_PSA$e,
#'   .costs = ShinyPSA::Brennan_1K_PSA$c,
#'   .params = ShinyPSA::Brennan_1K_PSA$p,
#'   .interventions = ShinyPSA::Brennan_1K_PSA$treats,
#'   .plot = TRUE)
#' }
summarise_PSA_ <- function(.effs, .costs, .params, .interventions = NULL,
                           .ref = NULL, .Kmax = 100000, .wtp = NULL,
                           .max_Kpoints = 100, .lambda = 30000, .plot = FALSE) {

  # Stop if .effs & .costs have different dimensions:
  stopifnot('Unequal dimensions in .effs and .costs' =
              dim(.effs) == dim(.costs),
            'PSA results for less than two interventions is supplied' =
              ncol(.effs) >= 2)

  # Simulations & interventions analysed:
  n.sim <- nrow(.effs) # Number of simulations
  n.comparators <- ncol(.effs) # Number of interventions
  n.comparisons <- n.comparators - 1 # Number of least possible comparisons
  v.ints <- 1:n.comparators # Vector with index of interventions'

  # Check supplied interventions labels, create ones if any is missing:
  if(!is.null(.interventions) & length(.interventions) != n.comparators) {
    .interventions <- NULL
  }
  if(is.null(.interventions)) {
    .interventions <- paste("intervention", 1:n.comparators)
  }

  # Set missing values or remove ones to be ignored:
  if(n.comparators == 2){
    # If no reference was provided in a non-incremental analysis:
    if(is.null(.ref)){
      .ref <- 1
      message(
        paste0(
          "You did not select a reference intervention. [",
          .interventions[.ref],
          "] will be used as reference for differential values and plots."
        )
      )
    }
    comp <- v.ints[-.ref]
  } else {
    # Ignore .ref if the analysis will be an incremental one:
    if(!is.null(.ref)) {
      .ref <- NULL
      message(
        "More than two interventions, .ref is ignored"
      )
    }
    comp <- NULL
  }

  # Set up willingness-to-pay:
  if (is.null(.Kmax)) {
    .Kmax <- 100000
  }
  if (is.null(.wtp)) {
    .wtp <- c(20000, 30000, 50000)
  }
  n.points <- .Kmax/.max_Kpoints
  v.k <- seq(from = 0, to = .Kmax, length.out = n.points + 1)
  v.k <- c(v.k, .wtp)
  v.k <- sort(unique(v.k))
  n.k <- length(v.k)
  names(v.k) <- scales::dollar(
    x = v.k,
    prefix = "\u00A3"
  )

  # Ensure .effs and .costs are tibbles and name columns appropriately:
  .effs <- .effs %>%
    dplyr::as_tibble(.name_repair = "unique") %>%
    `colnames<-`(.interventions)
  .costs <- .costs %>%
    dplyr::as_tibble(.name_repair = "unique") %>%
    `colnames<-`(.interventions)

  # Compute effects and costs differentials:
  if(n.comparators == 2) {
    delta.effs <- calculate_differentials_(
      .data = .effs,
      .ref = .ref
    )
    delta.costs <- calculate_differentials_(
      .data = .costs,
      .ref = .ref
    )
  } else {
    delta.effs <- NULL
    delta.costs <- NULL
  }

  # Compute ICER(s):
  ICER <- ShinyPSA::compute_ICERs_(
    .icer_data = NULL, .effs = .effs, .costs = .costs,
    .interventions = .interventions
  )

  # Compute NMB or iNMB, e.NMB or e.iNMB and best option for each k:
  nmbs <- ShinyPSA::compute_NMBs_(
    .effs = .effs, .costs = .costs, .interventions = .interventions,
    .Kmax = .Kmax, .wtp = .wtp
  )
  NMB <- nmbs$nmb
  e.NMB <- nmbs$e.nmb
  best <- nmbs$best_interv
  best_name <- nmbs$best_interv_name
  check <- nmbs$check
  kstar <- nmbs$wtp_star

  # Compute CEAC:
  CEAC <- ShinyPSA::compute_CEACs_(
    .nmb = NMB
  )

  # Compute CEAF:
  CEAF <- ShinyPSA::compute_CEAFs_(
    .ceac = CEAC
  )

  # Compute EVPI:
  EVPIs <- ShinyPSA::compute_EVPIs_(
    .effs = .effs, .costs = .costs, .Kmax = .Kmax,
    .interventions = .interventions, .wtp = .wtp
  )

  U <- EVPIs$U
  Ustar <- EVPIs$Ustar
  ol <- EVPIs$ol
  vi <- EVPIs$vi
  EVPI <- EVPIs$evi

  ## Outputs of the function:
  results <- list(

    interventions = .interventions, ref = .ref, comp = comp, ICER = ICER,
    NMB = NMB, e.NMB = e.NMB, CEAC = CEAC, CEAF = CEAF, EVPI = EVPI,
    best_id = best, best_name = best_name, WTPs = v.k,
    WTPstar = kstar, U = U, Ustar = Ustar, vi = vi, ol = ol, e = .effs,
    c = .costs, p = .params, delta.e = delta.effs, delta.c = delta.costs,
    n.sim = n.sim, n.comparators = n.comparators, step = n.k, Kmax = .Kmax
  )

  class(results) <- "psa"

  # If requested, develop and save plots and table:
  if(.plot == TRUE) {
    Summary_table <- ShinyPSA::draw_summary_table_(
      .PSA_data = results
    )
    CEP_plot <- ShinyPSA::plot_CEplane_(
      .PSA_data = results,
      .ref = .ref
    )
    CEAC_plot <- ShinyPSA::plot_CEAC_(
      .PSA_data = results,
      .ref = .ref
    )
    CEAF_plot <- ShinyPSA::plot_CEAF_(
      .PSA_data = results
    )
    EVPI_plot <- ShinyPSA::plot_EVPI_(
      .PSA_data = results
    )
    eNMB_plot <- ShinyPSA::plot_eNMB_(
      .PSA_data = results
    )
    stability_plots <- ShinyPSA::check_PSA_stability(
      .PSA_data = results
    )

    # Save plots/table to the final results object:
    results <- c(results,
                 'Summary_table' = list(Summary_table),
                 'CEP_plot' = list(CEP_plot),
                 'CEAC_plot' = list(CEAC_plot),
                 'CEAF_plot' = list(CEAF_plot),
                 'EVPI_plot' = list(EVPI_plot),
                 'eNMB_plot' = list(eNMB_plot),
                 'Stability_plots' = list(stability_plots))
  }

  return(results)
}
