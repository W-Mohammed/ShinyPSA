################################################################################
#
# Script Name:        compute_EVPIs.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of functions that aid EVPI computation.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

compute_EVPIs = function(.effs, .costs, .interventions = NULL, .Kmax = NULL,
                         .wtp = NULL) {
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

  # Compute monetary net benefit (NMB) (default):
  nmb <- array(rep(.effs, n.k) *
                 rep(v.k, each = n.sim * n.comparators) -
                 as.vector(.costs), # rep(.costs, n.k)
               dim = c(n.sim, n.comparators, n.k),
               dimnames = list(sims = NULL,
                               ints = .interventions,
                               k = NULL))

  # Compute expected net benefit (e.NMB):
  nmb <- aperm(nmb, c(3, 1, 2)) # array(slice = v.k, row = sims, col = v.ints)
  e.nmb <- apply(nmb, 3, function(x) apply(x, 1, mean))

  # Select the best option for each willingness-to-pay value:
  best <- max.col(e.nmb)

  # Compute opportunity loss (ol) and value-of-information (vi):
  nmb <- aperm(nmb, c(2, 1, 3)) # array(slice = sims, row = v.k, col = v.ints)
  Ustar <- vi <- ol <- matrix(NA, n.sim, n.k)
  for (i in 1:n.k) {
    Ustar[, i] <- get_row_max(nmb[, i,])
    cmd <- paste0("ol[, i] <- Ustar[, i] - nmb[, i,", best[i], "]")
    eval(parse(text = cmd))
    vi[, i] <- Ustar[, i] - max(apply(nmb[, i,], 2, mean))
  }
  evi <- colMeans(ol)

  return(list(Ustar = Ustar, vi = vi, ol = ol, evi = evi))
}
