################################################################################
#
# Script Name:        compute_EVPIs_.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of functions that aid EVPI computation.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Compute the Expected Value of Perfect Information (EVPI)
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
#'
#' @return A list containing the EVPI vector, value of information tibble,
#' opportunity lost tibble among others
#' @export
#'
#' @examples
#' \dontrun{}
compute_EVPIs_ = function(.effs, .costs, .interventions = NULL,
                          .Kmax = NULL, .wtp = NULL) {
  # Stop if .effs & .costs are not of class tibble or have unequal dims:
  stopifnot('.effs is not a tibble' = "data.frame" %in% class(.effs),
            '.costs is not a tibble' = "data.frame" %in% class(.costs),
            '.effs and .costs have unequal dimensions' =
              dim(.effs) == dim(.costs))

  # Simulations & interventions analysed:
  n.comparators <- ncol(.effs) # Number of interventions

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
    .Kmax <- 100000
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
    v.k <- c(v.k, 20000, 30000, 50000)
    v.k <- sort(unique(v.k))
    n.k <- length(v.k)
    names(v.k) <- paste0("£", format(v.k, big.mark = ","))
  }

  # Compute monetary net benefit (NMB) (default):
  nmb <- map2(.x = .effs, .y = .costs,
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

  # Identify the best option for each willingness-to-pay value:
  best_interv <- e.nmb %>%
    max.col(ties.method = "first")

  # Extract maximum nmb value at each iteration for each wtp/threshold:
  max_nmb_iter <- nmb %>%
    map_dfr(.f = function(.x) {
      do.call(pmax, as_tibble(.x, .name_repair = "unique"))
    })

  # Compute opportunity loss (OL):
  ol <- pmap_dfc(.l = list(nmb, best_interv, max_nmb_iter),
                 .f = function(.x, .y, .z) {
                   .z - .x[[.y]]
                 })

  # Compute value-of-information (VI):
  vi <- map2_dfc(.x = max_nmb_iter, .y = nmb,
                 .f = function(.x, .y) {
                   .x - max(colMeans(as_tibble(.y, .name_repair = "unique")))
                 })

  # Compute expected value-of-information (EVPI):
  evi <- colMeans(ol)

  return(list(U = nmb, Ustar = max_nmb_iter, vi = vi, ol = ol, evi = evi))
}
