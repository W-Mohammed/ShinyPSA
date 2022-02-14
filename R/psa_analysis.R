################################################################################
#
# Script Name:        psa_analysis.R
# Module Name:        Economic/PSA
# Script Description: Defines probabilistic sensitivity analysis functions.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Perform and report PSA analysis
#'
#' @param e A matrix containing the \code{effects} from PSA. Number of
#'  \code{columns} is equal to the interventions while the number of
#'  \code{rows} is equal to the number of PSA simulations to be analysed.
#' @param c A matrix containing the \code{costs} from PSA. Number of
#'  \code{columns} is equal to the interventions while the number of
#'  \code{rows} is equal to the number of PSA simulations to be analysed.
#' @param interventions A vector containing the names of all interventions.
#' If not provided or less names than needed is provided, the function will
#' generate generic names, for example \code{intervention 1}.
#' @param .ref An integer indicating the index of the reference
#' intervention. This index is used against the \code{intervention} vector.
#' @param Kmax The maximum willingness-to-pay threshold to use in the
#' analysis. This parameter is ignored if \code{wtp} is provided.
#' @param wtp A vector of \code{length > 0} identifying the
#' willingness-to-pay values to use in the analysis.
#' @param .incremental
#'
#' @return A list of class \code{psa} with \code{24} elements.
#' @export
#'
#' @examples
perform_psa <- function(e, c, .interventions = NULL, .ref = NULL,
                        .incremental = TRUE, .Kmax = 50000, .wtp = NULL) {

  # Stop if objects e & c are not of class matrix or different dimensions:
  stopifnot('is e a matrix' = "matrix" %in% class(e),
            'is c a matrix' = "matrix" %in% class(c),
            'e and c dimensions' = dim(e) == dim(c))

  # Simulations & interventions analysed:
  n.sim <- dim(e)[1] # Number of simulations
  n.comparators <- dim(e)[2] # Number of interventions
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
    n.points <- 500
    step <- .Kmax / n.points
    v.k <- seq(from = 0, to = .Kmax, by = step)
    n.k <- length(v.k)
  }

  # Compute ICER(s):
  ICER <- compute_icers(.icer_data = icer_df,
                        .incremental = .incremental, .ref = .ref)

  # Compute NMB or iNMB, e.NMB or e.iNMB and best option for each k:
  nmbs <- compute_NMBs(.effs = .effs, .costs = .costs, .ref = .ref,
                     .interventions = .interventions, .Kmax = .Kmax,
                     .wtp = .wtp)
  NMB <- nmbs$nmb
  e.NMB <- nmbs$e.nmb
  best <- nmbs$best
  check <- nmbs$check
  kstar <- nmbs$kstar

  # Compute CEAC:
  CEAC <- compute_CEACs(.nmb = NMB, .ref = .ref)

  # Compute CEAF:
  CEAF <- compute_CEAFs(.ceac = CEAC)

  # ICER(s), CEAC & NMB(s)/iNMB(s):
  if(n.comparisons == 1) { # If there were only two interventions:
    # Define comparator(s) and reference interventions:
    if(is.null(.ref)) {
      .ref = 1 # reference group default's to 1
    }
    v.comp <- v.ints[-.ref]

    # Compute Effectiveness & Cost differentials using ref intervention:
    delta.e <- e[, .ref] - e[, v.comp]
    delta.c <- c[, .ref] - c[, v.comp]
    dimnames(delta.e) <- dimnames(delta.c) <- list(NULL,
                                                   interventions[-.ref])

    # Compute the ICER with respect to the first intervention:
    ICER <- mean(delta.c) / mean(delta.e)

    # Compute CEAC & INMB:
    inmb <- scale(x = v.k %*% t(delta.e), center = delta.c, scale = FALSE)
    ceac <- rowMeans(inmb > 0)
    # Select the best option for each willingness-to-pay value:
    e.inmb <- rowMeans(inmb)
    best <- rep(.ref, n.k)
    best[which(e.inmb < 0)] <- v.comp
    # Finds the wtp value for which the optimal decision changes:
    check <- c(0, diff(best))
    kstar <- v.k[check != 0]

  } else if(n.comparisons > 1) { # multiple comparators
    # Incremental analysis:
    if(!is.null(.ref)) { # when a reference intervention is supplied
      # Define comparator(s) and reference interventions:
      v.comp <- v.ints[-.ref]

      # Compute Effectiveness & Cost differentials using .ref intervention:
      delta.e <- e[, .ref] - e[, v.comp]
      delta.c <- c[, .ref] - c[, v.comp]
      dimnames(delta.e) <- dimnames(delta.c) <- list(NULL,
                                                     interventions[-.ref])

      # Compute the ICER with respect to the ref intervention:
      ICER <- colMeans(delta.c) / colMeans(delta.e)

      # Compute INMB & CEAC:
      inmb <- array(rep(delta.e, n.k) *
                      rep(v.k, each = n.sim * n.comparisons) -
                      as.vector(delta.c), # rep(delta.c, n.k)
                    dim = c(n.sim, n.comparisons, n.k))
      inmb <- aperm(inmb, c(3, 1, 2)) # array(slice = k, row = sims, col = v.comps)
      ceac <- apply(inmb > 0, c(1, 3), mean)
      ceaf <- get_row_max(ceac)

      # Select the best option for each willingness-to-pay value:
      e.inmb <- apply(inmb, 3, function(x) apply(x, 1, mean))
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

    } else { # when a reference intervention is not supplied
      # Compute the ICER(s) of multiple comparators:
      icer_df <- tibble(
        'intervention' = interventions,
        'qalys' = colMeans(e),
        'costs' = colMeans(c)
      )
      ICER <- compute_icers(.icer_data = icer_df,
                            .incremental = .incremental, .ref = .ref)

      # Compute NMBs & CEAC:
      nmb <- array(rep(e, n.k) *
                     rep(v.k, each = n.sim * n.comparators) -
                     as.vector(c),
                   dim = c(n.sim, n.comparators, n.k))
      nmb <- aperm(nmb, c(3, 1, 2)) # array(slice = v.k, row = sims, col = v.comps)
      temp <- apply(nmb, 1, max.col)
      ceac <- matrix(NA, nrow = n.comparators, ncol = n.k)
      for (i in v.ints) {
        ceac[i, ] <- colMeans(temp == i)
      }
      ceac <- t(ceac)
      ceaf <- get_row_max(ceac)

      # Select the best option for each willingness-to-pay value:
      e.nmb <- apply(nmb, 3, function(x) apply(x, 1, mean))
      best <- max.col(e.nmb)
      # Finds the wtp value for which the optimal decision changes
      check <- c(0, diff(best))
      kstar <- v.k[check!=0]
    }
  }

  # Compute EVPI:
  U <- array(rep(e, n.k) *
               rep(v.k, each = n.sim * n.comparators) -
               as.vector(c),
             dim = c(n.sim, n.comparators, n.k))
  U <- aperm(U, c(1, 3, 2)) # array(slice = sims, row = v.k, col = v.ints)
  Ustar <- vi <- ol <- matrix(NA, n.sim, n.k)
  for (i in 1:n.k) {
    Ustar[, i] <- get_row_max(U[, i,])
    cmd <- paste("ol[, i] <- Ustar[, i] - U[, i,", best[i], "]",sep = "")
    eval(parse(text = cmd))
    vi[, i] <- Ustar[, i] - max(apply(U[, i,], 2, mean))
  }
  evi <- colMeans(ol)

  ## Outputs of the function
  results <- list(
    n.sim = n.sim, n.comparators = n.comparators,
    n.comparisons = n.comparisons,
    delta.e = if(!is.null(.ref)) as.data.frame(delta.e) else NULL,
    delta.c = if(!is.null(.ref)) as.data.frame(delta.c) else NULL,
    ICER = ICER, Kmax = Kmax, k = v.k, ceac = ceac,
    ib = if(!is.null(.ref)) inmb else nmb,
    eib = if(!is.null(.ref)) e.inmb else e.nmb,
    kstar = kstar, best = best, U = U, vi = vi, Ustar = Ustar, ol = ol,
    evi = evi, interventions = interventions, .ref = .ref,
    comp = if(!is.null(.ref)) v.comp else NULL,
    step = step, e = e, c = c
  )

  class(results) <- "psa"
  return(results)
}
