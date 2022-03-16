################################################################################
#
# Script Name:        summarise_PSA.R
# Module Name:        Economic/PSA
# Script Description: Defines a function that summarises probabilistic
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
#' @param .interventions A vector containing the names of all
#' interventions. If not provided or less names than needed is provided,
#' the function will generate generic names, for example
#' \code{intervention 1}.
#' @param .ref An integer indicating the index of the reference.
#' intervention. This index is used against the \code{intervention} vector.
#' @param .Kmax The maximum willingness-to-pay threshold to use in the
#' analysis. This parameter is ignored if \code{wtp} is provided.
#' @param .wtp A vector of \code{length > 0} identifying the
#' willingness-to-pay values to use in the analysis.
#' @return A list of class \code{psa} with \code{24} elements.
#' @export
#'
#' @examples
summarise_PSA_ <- function(.effs, .costs, .interventions = NULL,
                          .ref = NULL, .Kmax = 50000, .wtp = NULL,
                          .plot = FALSE) {

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

  # Associate .interventions with number IDs for cleaner plots' labels:
  .interventions <- paste0(1:length(.interventions),
                           ": ",
                           .interventions)

  # Set missing values or remove ones to be ignored:
  if(n.comparators == 2){
    # If no reference was provided in a non-incremental analysis:
    if(is.null(.ref)){
      .ref <- 1
      message(paste0("You did not select a reference intervention. [",
                    .interventions[.ref], "] will be used as reference for differential values and plots."))
    }
    comp <- v.ints[-.ref]
  } else {
    # Ignore .ref if the analysis will be an incremental one:
    if(!is.null(.ref)) {
      .ref <- NULL
      message("More than two interventions, .ref is ignored")
    }
    comp <- NULL
  }

  # Set up willingness-to-pay:
  if (is.null(.Kmax)) {
    .Kmax <- 50000
  }
  if (!is.null(.wtp)) {
    .wtp <- sort(unique(.wtp))
    .Kmax <- max(.wtp)
    v.k <- .wtp
    n.k <- length(.wtp)
    names(v.k) <- paste0("£", format(v.k, big.mark = ","))
  } else {
    n.points <- 500
    v.k <- seq(from = 0, to = .Kmax, length.out = n.points + 1)
    n.k <- length(v.k)
    names(v.k) <- paste0("£", format(v.k, big.mark = ","))
  }

  # Ensure .effs and .costs are tibbles and name columns appropriately:
  .effs <- .effs %>%
    as_tibble(.name_repair = "unique") %>%
    `colnames<-`(.interventions)
  .costs <- .costs %>%
    as_tibble(.name_repair = "unique") %>%
    `colnames<-`(.interventions)

  # Compute effects and costs differentials:
  if(n.comparators == 2) {
    delta.effs <- calculate_differentials_(.data = .effs, .ref = .ref)
    delta.costs <- calculate_differentials_(.data = .costs, .ref = .ref)
  } else {
    delta.effs <- NULL
    delta.costs <- NULL
  }

  # Compute ICER(s):
  ICER <- compute_ICERs_(.icer_data = NULL, .effs = .effs, .costs = .costs,
                         .interventions = .interventions)

  # Compute NMB or iNMB, e.NMB or e.iNMB and best option for each k:
  nmbs <- compute_NMBs_(.effs = .effs, .costs = .costs,
                        .interventions = .interventions, .Kmax = .Kmax,
                        .wtp = .wtp)
  NMB <- nmbs$nmb
  e.NMB <- nmbs$e.nmb
  best <- nmbs$best_interv
  best_name <- nmbs$best_interv_name
  check <- nmbs$check
  kstar <- nmbs$wtp_star

  # Compute CEAC:
  CEAC <- compute_CEACs_(.nmb = NMB)

  # Compute CEAF:
  CEAF <- compute_CEAFs_(.ceac = CEAC)

  # Compute EVPI:
  EVPIs <- compute_EVPIs_(.effs = .effs, .costs = .costs, .Kmax = .Kmax,
                          .interventions = .interventions, .wtp = .wtp)
  U <- EVPIs$U
  Ustar <- EVPIs$Ustar
  ol <- EVPIs$ol
  vi <- EVPIs$vi
  EVPI <- EVPIs$evi

  ## Outputs of the function
  results <- list(

    interventions = .interventions, ref = .ref, comp = comp,
    ICER = ICER, NMB = NMB, e.NMB = e.NMB, CEAC = CEAC, CEAF = CEAF,
    EVPI = EVPI, best_id = best, best_name = best_name, WTPs = v.k,
    WTPstar = kstar, U = U, Ustar = Ustar, vi = vi, ol = ol, e = .effs,
    c = .costs, delta.e = delta.effs, delta.c = delta.costs, n.sim = n.sim,
    n.comparators = n.comparators, step = n.k, Kmax = .Kmax
  )

  class(results) <- "psa"

  # If requested, develop and save plots:
  if(.plot == TRUE) {
    # Cost-Effectiveness plane:
    CEP_plot <- plot_CEplane_(.PSA_data = results, .ref = .ref)
    CEAC_plot <- plot_CEAC_(.PSA_data = results, .ref = .ref)
    CEAF_plot <- plot_CEAF_(.PSA_data = results)
    results <- c(results,
                 'CEP_plot' = list(CEP_plot),
                 'CEAC_plot' = list(CEAC_plot),
                 'CEAF_plot' = list(CEAF_plot))
  }

  return(results)
}
