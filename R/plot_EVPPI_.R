################################################################################
#
# Script Name:        plot_EVPPI_.R
# Module Name:        Economic/PSA
# Script Description: Defines EVPPI plotting function.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

plot_EVPPI_ <- function(pEVPI, .params) {
  makeEvppiBar(pEVPI[, 1], .params)
}

# plot_EVPPI_(pEVPI = evppi, .params = ShinyPSA::Brennan_1K_PSA$p)
