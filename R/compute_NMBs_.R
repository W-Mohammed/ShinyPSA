################################################################################
#
# Script Name:        compute_NMBs_.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of functions that estimate NMB, CEAC &
#                     CEAF.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Compute Monetary Net-Benefit (NMB) or incremental NMB (iNMB)
#'
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
#' @param .Kmax The maximum willingness-to-pay threshold to use in the
#' analysis. This parameter is ignored if \code{wtp} is provided.
#' @param .wtp A vector of numerical values declaring the
#' willingness-to-pay (WTP) values to use in the analysis. If \code{NULL}
#' (default) a range of WTP values (up to \code{.Kmax} will be used.
#' @param .max_Kpoints Maximum number of willingness-to-pay values (default
#' 100) to use in the analysis.
#'
#' @return A list containing the NMB list, eNMB tibble, WTP tibble and
#' other objects.
#' @export
#'
#' @examples
#' \dontrun{}
compute_NMBs_ <- function(.effs, .costs, .interventions = NULL,
                          .Kmax = NULL, .wtp = NULL, .max_Kpoints = 100) {
  # Stop if .effs & .costs are not of class tibble or have unequal dims:
  stopifnot('.effs is a not tibble' = "data.frame" %in% class(.effs),
            '.costs is a not tibble' = "data.frame" %in% class(.costs),
            '.effs and .costs have unequal dimensions' =
              dim(.effs) == dim(.costs))

  # Simulations & interventions analysed:
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

  # Name .effs and .costs columns appropriately:
  .effs <- .effs %>%
    `colnames<-`(.interventions)
  .costs <- .costs %>%
    `colnames<-`(.interventions)

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

  # Compute monetary net benefit (NMB) (default):
  nmb <- purrr::map2(
    .x = .effs,
    .y = .costs,
    .f = function(.eff = .x, .cost = .y) {
      purrr::map_dfc(
        .x = as.list(v.k),
        .f = function(.k = .x) {
          .eff * .k - .cost})}) %>%
    purrr::transpose()

  # Compute expected net benefit (e.NMB):
  e.nmb <- nmb %>%
    purrr::map_dfr(
      .f = function(.x) {
        colMeans(dplyr::as_tibble(.x, .name_repair = "unique"))
      })

  # Select the best option for each willingness-to-pay value:
  best_interv <- e.nmb %>%
    max.col(ties.method = "first")
  best_interv_name <- .interventions[best_interv]

  # Finds the wtp value for which the optimal decision changes
  check <- c(0, diff(best_interv))
  wtp_star <- v.k[check != 0]

  return(list(nmb = nmb, e.nmb = e.nmb, check = check, wtp_star = wtp_star,
              wtp = v.k, best_interv = best_interv,
              best_interv_name = best_interv_name))
}

#' Compute Cost-Effectiveness Acceptability Curve (CEAC)
#'
#' @param .nmb A list (with similar features to a 3D-array) containing the
#' Net Monetary Benefits from each probabilistic sensitivity analysis (PSA)
#' run for each intervention across a range of willingness-to-pay (WTP)
#' values. The dimensions of this list are:
#' \code{List:WTP, Tibble(Rows: PSA simulations, Cols: Interventions)}.
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
#' @param .Kmax The maximum willingness-to-pay threshold to use in the
#' analysis. This parameter is ignored if \code{wtp} is provided.
#' @param .wtp A vector of numerical values declaring the
#' willingness-to-pay (WTP) values to use in the analysis. If \code{NULL}
#' (default) a range of WTP values (up to \code{.Kmax} will be used.
#'
#' @return A tibble containing the probability of being cost-effective
#' for all interventions.
#' @export
#'
#' @examples
#' \dontrun{}
compute_CEACs_ <- function(.nmb, .effs = NULL, .costs = NULL,
                           .interventions = NULL, .Kmax = NULL,
                           .wtp = NULL) {
  # If .nmb was not available but raw data were:
  if(is.null(.nmb) & !is.null(.effs) & !is.null(.costs)){
    .nmb <- ShinyPSA::compute_NMBs_(
      .effs = .effs,
      .costs = .costs,
      .interventions = .interventions,
      .Kmax = .Kmax,
      .wtp = .wtp)
    .nmb <- .nmb$nmb
  }

  # Stop if object .nmb is not of class list:
  stopifnot('.nmb is not a list' = "list" %in% class(.nmb))

  # CEAC in incremental analysis:
  ceac <- .nmb %>%
    purrr::map_dfr(
      .f = function(.x) {
        colMeans(
          do.call(pmax, dplyr::as_tibble(.x, .name_repair = "unique")) ==
            dplyr::as_tibble(.x, .name_repair = "unique"))})

  return(ceac)
}

#' Compute Cost-Effectiveness Acceptability Frontier
#'
#' @param .ceac A tibble containing the probability of being cost-effective
#' for all interventions.
#' @param .nmb A list (with similar features to a 3D-array) containing the
#' Net Monetary Benefits from each probabilistic sensitivity analysis (PSA)
#' run for each intervention across a range of willingness-to-pay (WTP)
#' values. The dimensions of this list are:
#' \code{List:WTP, Tibble(Rows: PSA simulations, Cols: Interventions)}.
#'
#' @return A tibble containing the probability of being cost-effective
#' for all interventions alongside the CEAF.
#' @export
#'
#' @examples
#' \dontrun{}
compute_CEAFs_ <- function(.ceac, .nmb = NULL) {
  # Stop if object .ceac is not of class tibble:
  stopifnot('.ceac is a not tibble' = "data.frame" %in% class(.ceac))
  if(!is.null(.nmb))
    stopifnot('.nmb is not a list' = "list" %in% class(.nmb))

  # If .ceac was not available but .nmb was:
  if(is.null(.ceac) & !is.null(.nmb))
    .ceac <- ShinyPSA::compute_CEACs_(.nmb = .nmb)

  # Compute CEAF:
  ceaf <- .ceac %>%
    dplyr::mutate('ceaf' = do.call(pmax, .))

  return(ceaf)
}
