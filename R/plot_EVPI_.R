################################################################################
#
# Script Name:        plot_EVPI_.R
# Module Name:        Economic/PSA
# Script Description: Defines the function that plots the Expected Value
#                     of Perfect Information (EVPI)
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Plot the Expected Value of Perfect Information (EVPI)
#'
#' @param .PSA_data A list of class shinyPSA that contains summary PSA
#' results.
#' @param ... Additional arguments that include:
#' legend position \code{.legend_pos = "bottom"},
#' willingness-to-pay threshold(s) \code{.wtp_threshold = c(20000, 30000)},
#' show WTP threshold(s) lines \code{.show_wtp = TRUE},
#' show WTP threshold(s) labels \code{.label_wtp = TRUE},
#' plot individual EVPI \code{.individual_evpi = TRUE},
#' time horizon to estimate population EVPI \code{.time_horion = 5},
#' discount rate to estimate population EVPI \code{.discount_rate = 0.035},
#' population size for population EVPI \code{.population = 15000},
#' zoom to min/max values \code{.zoom = FALSE}, and
#' zoom to supplied coordinates values \code{.zoom_cords = NULL}.
#'
#' @return An object of class ggplot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ShinyPSA)
#'
#' PSA_summary <- summarise_PSA_(
#'   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
#'   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
#'   .interventions = ShinyPSA::Smoking_PSA$treats)
#'
#' p <- plot_EVPI_(PSA_summary,
#'                .legend_pos = NULL,
#'                .wtp_threshold = c(2000, 10000, 20000, 25000),
#'                .show_wtp = TRUE,
#'                .label_wtp = FALSE,
#'                .individual_evpi = FALSE,
#'                .time_horion = 1,
#'                .discount_rate = 0.035,
#'                .population = 15000,
#'                .zoom = FALSE,
#'                .zoom_cords = NULL)
#'
#' p
#' }
#'
plot_EVPI_ <- function(.PSA_data, ...) {
  # Grab the function's environment for correct assignment in assign():
  env_ = environment()
  # Define defaults:
  default_args <- list(
    '.legend_pos' = "bottom", # c(x, y) double between 0:1 or character
    '.wtp_threshold' = c(20000, 30000),
    '.show_wtp' = TRUE, # TRUE/FALSE
    '.label_wtp' = FALSE, # TRUE/FALSE
    '.individual_evpi' = TRUE, # TRUE/FALSE
    '.time_horion' = 5, # Integer
    '.discount_rate' = 0.035, # double 0:1
    '.population' = 15000, # Integer
    '.zoom' = FALSE, # TRUE/FALSE
    '.zoom_cords' = NULL) # c(x, x) double min and max x axis values
  # Grab additional arguments:
  args_ <- list(...)
  # Assign additional arguments:
  ShinyPSA::assign_extraArgs_(.default_args_ = default_args,
                    .args_ = args_,
                    .env_ = env_)

  # Plot data:
  discounted_population = 1
  subtitle_lab = "    Individual EVPI"

  ## Population EVPI:
  if(!.individual_evpi){
    ## Population EVPI:
    discounted_population <- sum(
      .population / ((1 + .discount_rate)^(1:.time_horion)))
    subtitle_lab = paste0("    Population EVPI: [",
                          "Population size: ", .population, "; ",
                          "Time horizon: ", .time_horion, " year(s); ",
                          "Discount rate: ", .discount_rate, ".]")
  }
  ## EVPI data:
  evpi_df <- dplyr::tibble('EVPI' = .PSA_data$EVPI * discounted_population,
                    'WTP threshold' = .PSA_data$WTPs,
                    'Best option' = .PSA_data$best_name)

  # Zoom:
  y_cords <- NULL
  if(.zoom | (!is.null(.zoom_cords) & is.numeric(.zoom_cords))) {
    .zoom = TRUE
    if(is.null(.zoom_cords) |
       (!is.null(.zoom_cords) & length(.zoom_cords) != 2))
      .zoom_cords = c(0, 31000)
  }

  # CEAC main plot:
  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(ylim = y_cords, xlim = .zoom_cords, expand = FALSE) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = 'grey',
      size = 0.1) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = 'grey',
      size = 0.1) +
    ggplot2::geom_line(
      data = evpi_df,
      ggplot2::aes(x = `WTP threshold`,
          y = EVPI),
      size = 0.4) +
    ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
    ggplot2::theme(
      # Adjust title size and position:
      plot.title.position = "plot", # Start title from near the margin
      plot.subtitle = ggplot2::element_text(size = 6, face = "italic"),
      legend.position = .legend_pos,
      legend.title = ggplot2::element_blank(),
      # Control legend text alignment:
      legend.text.align = 0, # 0 left (default), 1 right
      # Remove background and box around the legend:
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.spacing = ggplot2::unit(0, "cm"), # spacing between legend items
      legend.spacing.y = ggplot2::unit(-0.195, "cm"), # bring legends closer
      legend.margin=ggplot2::margin(t = -8), # remove space between it x-axis
      # Add a box around the keys:
      legend.key = ggplot2::element_rect(fill = "white", colour = "grey"),
      legend.key.size = ggplot2::unit(0.35, "cm"),
      # Add a border and space around the plot:
      panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
      plot.margin = ggplot2::unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                         c("points", "cm", "points", "points"))) +
    ggplot2::labs(
      title = "Expected Value of Perfect Information (EVPI)",
      x = "Willingness-to-pay (£)",
      y = "Expected value of perfect information",
      subtitle = subtitle_lab)

  # Show/hide WTP on the CEAF:
  if(.show_wtp) {
    ## CEAF plot willingness-to-pay (WTP) values:
    .wtp = .wtp_threshold %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        x_cord = .wtp_threshold,
        y_cord = max(evpi_df$EVPI),
        angle_cord = 0,
        label_cord = paste0("£", format(.wtp_threshold,
                                        big.mark = ",")),
        lty_ = "Willingness-to-pay (£)")

    ## Plot:
    p <- p +
      ggplot2::geom_vline(
        data = .wtp,
        ggplot2::aes(xintercept = x_cord,
            linetype = lty_),
        colour = "dark gray") +
      ggplot2::scale_linetype_manual(
        breaks = .wtp$lty_[1], # keep one for the legend
        values = rep(3, nrow(.wtp))) +
      ggplot2::guides(
        # Remove the shapes from the line:
        linetype = ggplot2::guide_legend(
          override.aes = list(order = 2,
                              shape = NA, # remove shape
                              color = 'black')))
  }

  # Label WTP value(s) on the CEAF:
  if(.label_wtp) {
    p <- p +
      ggrepel::geom_text_repel(
        data = .wtp,
        ggplot2::aes(x = x_cord,
            y = y_cord,
            angle = angle_cord,
            label = label_cord),
        size = 1.5,
        show.legend = FALSE)
  }

  return(p)
}
