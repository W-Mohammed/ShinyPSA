################################################################################
#
# Script Name:        compute_NMBs.R
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
compute_NMBs <- function(.effs, .costs, .ref = NULL, .interventions = NULL,
                         .Kmax = NULL, .wtp = NULL) {
  # Stop if .effs & .costs are not of class matrix or different dimensions:
  stopifnot('is .effs a matrix' = "matrix" %in% class(.effs),
            'is .costs a matrix' = "matrix" %in% class(.costs),
            '.effs and .costs dimensions' = dim(.effs) == dim(.costs))

  # Simulations & interventions analysed:
  n.sim <- dim(.effs)[1] # Number of simulations
  n.comparators <- dim(.effs)[2] # Number of interventions
  n.comparisons <- n.comparators - 1 # Number of least possible comparisons
  v.ints <- 1:n.comparators # Vector with index of interventions'

  # Check supplied interventions labels, create ones if any is missing:
  if(!is.null(.interventions) & length(.interventions) != n.comparators) {
    .interventions <- NULL
  }
  if(is.null(.interventions)) {
    .interventions <- paste("intervention", 1:n.comparators)
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
    n.points <- .Kmax/100
    step <- .Kmax / n.points
    v.k <- seq(from = 0, to = .Kmax, by = step)
    n.k <- length(v.k)
  }

  # Compute incremental monetary net benefit iNMB (only if .ref != NULL):
  if(!is.null(.ref)) {
    # Define comparator(s) and reference interventions:
    v.comp <- v.ints[-.ref]

    # Compute Effectiveness & Cost differentials using .ref intervention:
    delta.e <- .effs[, .ref] - .effs[, v.comp]
    delta.c <- .costs[, .ref] - .costs[, v.comp]
    dimnames(delta.e) <- dimnames(delta.c) <- list(NULL,
                                                   .interventions[-.ref])

    # Calculate iNMB:
    inmb <- array(rep(delta.e, n.k) *
                    rep(v.k, each = n.sim * n.comparisons) -
                    as.vector(delta.c), # rep(delta.c, n.k)
                  dim = c(n.sim, n.comparisons, n.k),
                  dimnames = list(sims = NULL,
                                  ints = .interventions[-.ref],
                                  k = NULL))
    inmb <- aperm(inmb, c(3, 1, 2)) # array(slice = k, row = sims, col = v.comps)

    # Compute incremental expected net benefit e.iNMB:
    e.inmb <- apply(inmb, 3, function(x) apply(x, 1, mean))

    # Select the best option for each willingness-to-pay value:
    if (is.null(dim(e.inmb))) {
      tmp <- min(e.inmb)
      tmp2 <- which.min(e.inmb)
    } else {
      tmp <- apply(e.inmb, 1, min)
      tmp2 <- apply(e.inmb, 1, which.min)
    }
    best <- ifelse(tmp > 0, .ref, v.comp[tmp2])

    # Finds the wtp value for which the optimal decision changes
    check <- c(0, diff(best))
    kstar <- v.k[check!=0]

    return(list(nmb = inmb, e.nmb = e.inmb, best = best, check = check,
                kstar = kstar))
  }
  # Compute monetary net benefit (NMB) (default):
  nmb <- array(rep(.effs, n.k) *
                 rep(v.k, each = n.sim * n.comparators) -
                 as.vector(.costs), # rep(.costs, n.k)
               dim = c(n.sim, n.comparators, n.k),
               dimnames = list(sims = NULL,
                               ints = .interventions,
                               k = NULL))
  nmb <- aperm(nmb, c(3, 1, 2)) # array(slice = v.k, row = sims, col = v.comps)

  # Compute expected net benefit (e.NMB):
  e.nmb <- apply(nmb, 3, function(x) apply(x, 1, mean))

  # Select the best option for each willingness-to-pay value:
  best <- max.col(e.nmb)

  # Finds the wtp value for which the optimal decision changes
  check <- c(0, diff(best))
  kstar <- v.k[check!=0]

  return(list(nmb = nmb, e.nmb = e.nmb, best = best, check = check,
              kstar = kstar))
}

#' Compute Cost-Effectiveness Acceptability Curve
#'
#' @param .nmb
#' @param .ref
#' @param .effs
#' @param .costs
#' @param .interventions
#' @param .Kmax
#' @param .wtp
#'
#' @return
#' @export
#'
#' @examples
compute_CEACs <- function(.nmb, .effs = NULL, .costs = NULL, .ref = NULL,
                          .interventions = NULL, .Kmax = NULL, .wtp = NULL) {
  # If .nmb was not available but raw data were:
  if(is.null(.nmb) & !is.null(.effs) & !is.null(.costs))
    .nmb <- compute_NMBs(.effs = .effs, .costs = .costs, .ref = .ref)$nmb

  # Stop if object .nmb is not of class matrix:
  stopifnot('is .nmb a array' = "array" %in% class(.nmb))

  # If we had incremental monetary net benefit iNMB (only if .ref != NULL):
  if(!is.null(.ref)) {
    ceac <- apply(.nmb > 0, c(1, 3), mean)

  } else {
    n.k <- dim(.nmb)[1] # number of threshold/MAICER values
    n.comparators <- dim(.nmb)[3]
    v.ints <- 1:n.comparators
    # With monetary net benefit (NMB) (default):
    temp <- apply(.nmb, 1, max.col, ties.method = "first")
    ceac <- matrix(data = NA, nrow = n.comparators, ncol = n.k,
                   dimnames = list(ints = dimnames(.nmb)$ints,
                                   k = NULL))
    for (i in v.ints) {
      ceac[i, ] <- colMeans(temp == i)
    }
    ceac <- t(ceac)
  }

  return(ceac)
}

#' Compute Cost-Effectiveness Acceptability Frontier
#'
#' @param .ceac
#' @param .nmb
#' @param .ref
#'
#' @return
#' @export
#'
#' @examples
compute_CEAFs <- function(.ceac, .nmb = NULL, .ref = NULL) {
  # If .ceac was not available but .nmb was:
  if(is.null(.ceac) & !is.null(.nmb))
    .ceac <- compute_CEACs(.nmb = .nmb, .ref = .ref)

  # Stop if object .ceac is not of class matrix:
  stopifnot('is .ceac a matrix' = "matrix" %in% class(.ceac))

  ceaf <- get_row_max(.ceac)

  return(ceaf)
}
