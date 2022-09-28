################################################################################
#
# Script Name:        compute_EVPPIs_.R
# Module Name:        Economic/PSA
# Script Description: Defines EVPPIs estimating function.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

compute_EVPPIs_ <- function(.effs, .costs, .params, .lambda, .session = NULL) {

  inb <- createInb(
    .costs,
    .effs,
    .lambda)

  pEVPI <- applyCalcSingleParamGam(
    .params = .params,
    nb = inb,
    session = .session)

}

# evppi = compute_EVPPIs_(.effs = ShinyPSA::Brennan_1K_PSA$e,
#                         .costs = ShinyPSA::Brennan_1K_PSA$c,
#                         .params = ShinyPSA::Brennan_1K_PSA$p,
#                         .lambda = 30000)
