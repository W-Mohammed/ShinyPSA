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
#' @param .incremental A logical value of whether an incremental analysis
#' should be conducted. Default value is \code{TRUE}
#'
#' @return A list of class \code{psa} with \code{24} elements.
#' @export
#'
#' @examples
summarise_PSA <- function(.effs, .costs, .interventions = NULL,
                          .ref = NULL, .incremental = TRUE, .Kmax = 50000,
                          .wtp = NULL) {

  # Stop if objects e & c are not of class matrix or different dimensions:
  stopifnot('unequal dimensions in .effs and .costs' =
              dim(.effs) == dim(.costs))

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
  if(!.incremental){
    # If no reference was provided in a non-incremental analysis:
    if(is.null(.ref)){
      message(paste("No reference was provided, using ",
                    .interventions[.ref], "as reference."))
      .ref <- 1
    }
    v.comp <- v.ints[-.ref]
  } else {
    # Ignore .ref if the analysis was incremental:
    if(!is.null(.ref)) {
      message(".incremental is set to TRUE, .ref is ignored")
      .ref <- NULL
    }
    v.comp <- NULL
  }

  # Set up willingness-to-pay:
  if (is.null(.Kmax)) {
    .Kmax <- 50000
  }
  if (!is.null(.wtp)) {
    .wtp <- sort(unique(.wtp))
    .Kmax <- max(.wtp)
    step <- NA
    v.k <- .wtp
    n.k <- length(.wtp)
  } else {
    n.points <- 500
    step <- .Kmax / n.points
    v.k <- seq(from = 0, to = .Kmax, by = step)
    n.k <- length(v.k)
  }

  # Ensure .effs and .costs are tibbles and name columns appropriately:
  .effs <- .effs %>%
    as_tibble() %>%
    `colnames<-`(.interventions)
  .costs <- .costs %>%
    as_tibble() %>%
    `colnames<-`(.interventions)

  # Compute effects and costs differentials, if .incremental = FALSE:
  if(.incremental) {
    delta.effs <- NULL
    delta.costs <- NULL
  } else {
    delta.effs <- .effs %>%
      select(-.ref) %>%
      mutate(across(.fns =
                      ~ .effs %>%
                      select(all_of(.ref)) - .x))

    delta.costs <- .costs %>%
      select(-.ref) %>%
      mutate(across(.fns =
                      ~ .costs %>%
                      select(all_of(.ref)) - .x))
  }

  # Compute ICER(s):
  ICER <- compute_ICERs_(.icer_data = NULL, .effs = .effs, .costs = .costs,
                         .interventions = .interventions, .ref = .ref,
                         .incremental = .incremental)

  # Compute NMB or iNMB, e.NMB or e.iNMB and best option for each k:
  nmbs <- compute_NMBs_(.effs = .effs, .costs = .costs, .ref = .ref,
                        .interventions = .interventions, .Kmax = .Kmax,
                        .wtp = .wtp)
  NMB <- nmbs$nmb
  e.NMB <- nmbs$e.nmb
  best <- nmbs$best_interv
  best_name <- nmbs$best_interv_name
  check <- nmbs$check
  kstar <- nmbs$kstar

  # Compute CEAC:
  CEAC <- compute_CEACs_(.nmb = NMB, .ref = .ref)

  # Compute CEAF:
  CEAF <- compute_CEAFs_(.ceac = CEAC, .ref = .ref)

  # Compute EVPI:
  EVPIs <- compute_EVPIs_(.effs = .effs, .costs = .costs, .Kmax = .Kmax,
                          .interventions = .interventions, .wtp = .wtp)
  EVPI <- EVPIs$evi
  vi <- EVPIs$vi
  Ustar <- EVPIs$Ustar
  ol <- EVPIs$ol

  ## Outputs of the function
  results <- list(
    n.sim = n.sim, n.comparators = n.comparators,
    n.comparisons = n.comparisons, delta.e = delta.effs,
    delta.c = delta.costs, ICER = ICER, Kmax = .Kmax, k = v.k, NMB = NMB,
    e.NMB = e.NMB, CEAC = CEAC, CEAF = CEAF, EVPI = EVPI, kstar = kstar,
    best = best, U = NMB, vi = vi, Ustar = Ustar, ol = ol, step = step,
    interventions = .interventions, .ref = .ref, comp = v.comp,
    e = .effs, c = .costs
  )

  class(results) <- "psa"
  return(results)
}
