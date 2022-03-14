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
#' @param .effs
#' @param .costs
#' @param .ref
#' @param .interventions
#' @param .Kmax
#' @param .wtp
#'
#' @return
#' @export
#'
#' @examples
compute_NMBs_ <- function(.effs, .costs, .ref = NULL,
                          .interventions = NULL, .Kmax = NULL,
                          .wtp = NULL) {
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
    # Associate .interventions with number IDs for cleaner plots' labels:
    .interventions <- paste0(1:length(.interventions),
                             ": ",
                             .interventions)
  }

  # Name .effs and .costs columns appropriately:
  .effs <- .effs %>%
    `colnames<-`(.interventions)
  .costs <- .costs %>%
    `colnames<-`(.interventions)

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
    n.points <- .Kmax/100
    v.k <- seq(from = 0, to = .Kmax, length.out = n.points + 1)
    n.k <- length(v.k)
    names(v.k) <- paste0("£", format(v.k, big.mark = ","))
  }

  delta.effs <- NULL
  delta.costs <- NULL

  # Compute incremental monetary net benefit iNMB:
  if(n.comparators == 2) {
    # Set reference to 1 if undefined by the user:
    if(is.null(.ref)) .ref = 1

    # Define comparator index and reference interventions:
    .comp <- v.ints[-.ref]

    # Compute effects and costs differentials:
    delta.effs <- .effs %>%
      select(-.ref) %>%
      mutate(across(.fns = function(.x) .x - .effs %>%
                      pull(.ref)))

    delta.costs <- .costs %>%
      select(-.ref) %>%
      mutate(across(.fns = function(.x) .x - .costs %>%
                      pull(.ref)))

    # Calculate iNMB:
    inmb <- map2(.x = delta.effs,
                 .y = delta.costs,
                 .f = function(.eff = .x, .cost = .y) {
                   map_dfc(as.list(v.k),
                           .f = function(.k = .x) {
                             .eff * .k - .cost})}) %>%
      transpose()

    # Compute incremental expected net benefit e.iNMB:
    e.inmb <- inmb %>%
      map_dfr(.f = function(.x) {
        colMeans(as_tibble(.x, .name_repair = "unique"))
      })

    # Select the best option for each willingness-to-pay value:
    best_interv <- e.inmb %>%
      #do.call(pmax, .) %>%
      {ifelse(. < 0, .ref, .comp)}
    best_interv_name <- .interventions[best_interv]

    # Finds the wtp value for which the optimal decision changes
    check <- c(0, diff(best_interv))
    wtp_star <- v.k[check != 0]

    return(list(nmb = inmb, e.nmb = e.inmb, check = check, ref = .ref,
                wtp_star = wtp_star, wtp = v.k, delta.effs = delta.effs,
                delta.costs = delta.costs, best_interv = best_interv,
                best_interv_name = best_interv_name))
  }

  # Compute monetary net benefit (NMB) (default):
  nmb <- map2(.x = .effs,
              .y = .costs,
              .f = function(.eff = .x, .cost = .y) {
                map_dfc(as.list(v.k),
                        .f = function(.k = .x) {
                          .eff * .k - .cost})}) %>%
    transpose()

  # Compute expected net benefit (e.NMB):
  e.nmb <- nmb %>%
    map_dfr(.f = function(.x) {
      colMeans(as_tibble(.x, .name_repair = "unique"))
    })

  # Select the best option for each willingness-to-pay value:
  best_interv <- e.nmb %>%
    max.col(ties.method = "first")
  best_interv_name <- .interventions[best_interv]

  # Finds the wtp value for which the optimal decision changes
  check <- c(0, diff(best_interv))
  wtp_star <- v.k[check != 0]

  return(list(nmb = nmb, e.nmb = e.nmb, check = check, ref = .ref,
              wtp_star = wtp_star, wtp = v.k, delta.effs = delta.effs,
              delta.costs = delta.costs, best_interv = best_interv,
              best_interv_name = best_interv_name))
}

#' Compute Cost-Effectiveness Acceptability Curve (CEAC)
#'
#' @param .nmb
#' @param .effs
#' @param .costs
#' @param .ref
#' @param .interventions
#' @param .Kmax
#' @param .wtp
#'
#' @return
#' @export
#'
#' @examples
compute_CEACs_ <- function(.nmb, .effs = NULL, .costs = NULL, .ref = NULL,
                           .interventions = NULL, .Kmax = NULL,
                           .wtp = NULL) {
  # If .nmb was not available but raw data were:
  if(is.null(.nmb) & !is.null(.effs) & !is.null(.costs)){
    .nmb <- compute_NMBs_(.effs = .effs, .costs = .costs, .ref = .ref,
                          .interventions = .interventions, .Kmax = .Kmax,
                          .wtp = .wtp)
    .ref <- .nmb$ref # Grab ref if only one comparator
    .nmb <- .nmb$nmb
  }

  # Stop if object .nmb is not of class list:
  stopifnot('.nmb is not a list' = "list" %in% class(.nmb))

  # If we had incremental monetary net benefit iNMB (only if .ref != NULL):
  if(!is.null(.ref)) {
    ceac <- .nmb %>%
      map_dfr(.f = function(.x) {
        colMeans(as_tibble(.x, .name_repair = "unique")) >= 0
      })
  } else {
    # CEAC in incremental analysis:
    ceac <- .nmb %>%
      map_dfr(.f = function(.x) {
        colMeans(do.call(pmax, as_tibble(.x, .name_repair = "unique")) ==
                   as_tibble(.x, .name_repair = "unique"))})
  }

  return(ceac)
}

#' Compute Cost-Effectiveness Acceptability Frontier
#'
#' @param .ceac
#' @param .nmb
#'
#' @return
#' @export
#'
#' @examples
compute_CEAFs_ <- function(.ceac, .nmb = NULL) {
  # Stop if object .ceac is not of class tibble:
  stopifnot('.ceac is a not tibble' = "data.frame" %in% class(.ceac))
  if(!is.null(.nmb))
    stopifnot('.nmb is not a list' = "list" %in% class(.nmb))

  # If .ceac was not available but .nmb was:
  if(is.null(.ceac) & !is.null(.nmb))
    .ceac <- compute_CEACs_(.nmb = .nmb)

  # Compute CEAF when .ceac is based on incremental analysis .ref = NULL:
  ceaf <- .ceac %>%
    mutate('ceaf' = if(any(rowSums(.) != 1)) NA_real_
           else do.call(pmax, .))

  return(ceaf)
}
