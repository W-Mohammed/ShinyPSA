################################################################################
#
# Script Name:        summarise_EVPPI_.R
# Module Name:        Economic/PSA
# Script Description: Defines EVPPI tabulating and plotting functions.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

plot_EVPPI_ <- function(.PSA_data = NULL, EVPPI_res) {
  # Prepare plot data:
  EVPPI_data <- EVPPI_res[[1]] %>%
    dplyr::select(c(1, 2)) %>%
    `colnames<-`(c("Parameters", "Per Person EVPPI")) %>%
    dplyr::arrange(dplyr::desc(`Per Person EVPPI`))

  p <- ggplot2::ggplot(data = EVPPI_data) +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = `Per Person EVPPI`,
        y = Parameters),
      stat = "identity",
      position = ggplot2::position_dodge()) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = 'grey',
      size = 0.1) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = 'grey',
      size = 0.1) #+
    # ggplot2::coord_flip()

  return(p)

  # makeEvppiBar(pEVPI[, 1], .params)
}

# plot_EVPPI_(pEVPI = evppi, .params = ShinyPSA::Brennan_50K_PSA$p)
