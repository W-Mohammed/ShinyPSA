################################################################################
#
# Script Name:        plot_EVPPI_.R
# Module Name:        Economic/PSA
# Script Description: Defines EVPPI plotting function.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Plot the Expected Value of Perfect Partial Information (EVPPI) results.
#'
#' @param EVPPI_res A list containing EVPPI results.
#' @param .show_percent Bolean for whether to show the percentage of overall
#' EVPI on the bars.
#' @param .min_percent Only parameters with percentage of overall EVPI equal to
#' or higher than .min_percent will make it to the plot.
#' @param .params_num The number of parameters to show in the bar plot.
#'
#' @return An Object of class ggplot2.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ShinyPSA)
#'
#' # Summarise PSA results:
#' PSA_summary = summarise_PSA_(
#'   .effs = ShinyPSA::Brennan_1K_PSA$e,
#'   .costs = ShinyPSA::Brennan_1K_PSA$c,
#'   .params = ShinyPSA::Brennan_1K_PSA$p,
#'   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
#'
#' # Estimate EVPPI:
#' EVPPI_ind_res <- compute_EVPPIs_(
#'   .PSA_data = PSA_summary,
#'   .MAICER_ = 30000)
#'
#' # Plot EVPPI results:
#' plot_EVPPI_(
#'   EVPPI_res = EVPPI_ind_res,
#'   .params_num = 15,
#'   .min_percent = NULL)
#' }
plot_EVPPI_ <- function(EVPPI_res, .show_percent = TRUE, .min_percent = 1,
                        .params_num = NULL) {
  # Prepare plot data:----
  EVPPI_data <- EVPPI_res[[1]] %>%
    ## subset data columns:----
    `colnames<-`(c("Parameters", "Per Person EVPPI", colnames(.)[-c(1,2)])) %>%
    ## create a numeric percentage EVPPI of overall EVPI:----
    dplyr::mutate(
      'percent' = as.numeric(
        gsub(pattern = '%',
             replacement = "",
             x = EVPPI_res[[1]][[4]]))) %>%
    ## rename exiting string percent variable to a usable name:----
    dplyr::rename("Percent overall EVPI" = "Indexed to Overall EVPI (%)") %>%
    ## sort EVPPI values in descending order:----
    dplyr::arrange(dplyr::desc(`Per Person EVPPI`)) %>%
    ## if a subset of EVPPI is requested:----
    {if(is.null(.params_num)) {
      .
    } else {
      if(is.numeric(.params_num)) {
      dplyr::slice_head(.data = ., n = .params_num)
      } else {
        .}}} %>%
    ## if a user wants :----
    {if(is.null(.min_percent)) {
      .
    } else {
      if(is.numeric(.min_percent)) {
        dplyr::filter(.data = ., percent >= .min_percent)
      } else {
        .}}}

  # Build EVPPI plot:----
  p <- ggplot2::ggplot(data = EVPPI_data) +
    ggplot2::coord_cartesian(xlim = c(0, NA)) +
    ## add bar plot:----
    ggplot2::geom_bar(
      ggplot2::aes(
        x = `Per Person EVPPI`,
        y = forcats::fct_reorder(Parameters, `Per Person EVPPI`)),
      fill = "#EFC00099", #"#EFC000FF"
      stat = "identity",
      position = ggplot2::position_dodge2()) +
    ## add axis lines:----
    ggplot2::geom_hline(
      yintercept = 0,
      color = 'grey',
      size = 0.1) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = 'grey',
      size = 0.1) +
    ## fine tuning the plot:----
    ggplot2::scale_x_continuous(labels = scales::dollar_format(
      prefix = "\u00A3")) +
    ggplot2::theme(
      # axis.ticks.length.y = element_text(hjust = -2),
      plot.title.position = "plot", # Start title from near the margin
      ### Add a border and space around the plot:
      # panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
      plot.margin = ggplot2::unit(c(5.5, 0.5, 5.5, 5.5), # more space LHS
                                  c("points", "cm", "points", "points"))) +
      # panel.grid = element_blank(),
      # panel.border = element_blank()) +
    ggplot2::labs(
      title = "Per Person EVPPI",
      caption = EVPPI_res[['Plot caption']],
      x = "EVPPI (\u00A3)",
      y = "Parameters")

  # If user wants to see percentages on bars:
  if(isTRUE(.show_percent)) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(
          x = `Per Person EVPPI`,
          y = forcats::fct_reorder(Parameters, `Per Person EVPPI`),
          label = `Percent overall EVPI`),
        ## reduce label size with large number of params
        size = if(nrow(EVPPI_data) > 15) 2.5 else NA)
  }

  return(p)

}
