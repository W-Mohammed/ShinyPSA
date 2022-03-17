################################################################################
#
# Script Name:        plot_EVPI_.R
# Module Name:        Economic/PSA
# Script Description: Defines a functions that plots the Expected Value
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
#' show WTP threshold(s) labels \code{.label_wtp' = TRUE},
#' plot individual EVPI \code{.individual_evpi' = TRUE},
#' time horizon to estimate population EVPI \code{.time_horion = 5},
#' discount rate to estimate population EVPI \code{.discount_rate = 0.035},
#' population size for population EVPI \code{.population' = 15000},
#' zoom to min/max values \code{.zoom = FALSE}, and
#' zoom to supplied coordinates values \code{.zoom_cords = NULL}.
#'
#' @return An object of class ggplot.
#' @export
#'
#' @examples
#' library(ShinyPSA)
#' PSA_summary = summarise_PSA_(
#'   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
#'   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
#'   .interventions = ShinyPSA::Smoking_PSA$treats)
#' p = plot_EVPI_(PSA_summary,
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
#' p
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
  assign_extraArgs_(.default_args_ = default_args,
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
  evpi_df <- tibble('EVPI' = .PSA_data$EVPI * discounted_population,
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
  p <- ggplot() +
    coord_cartesian(ylim = y_cords, xlim = .zoom_cords, expand = FALSE) +
    geom_hline(
      yintercept = 0,
      color = 'grey',
      size = 0.1) +
    geom_vline(
      xintercept = 0,
      color = 'grey',
      size = 0.1) +
    geom_line(
      data = evpi_df,
      aes(x = `WTP threshold`,
          y = EVPI),
      size = 0.4) +
    scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
    labs(
      title = "Expected Value of Perfect Information (EVPI)",
      x = "Willingness-to-pay (£)",
      y = "Expected value of perfect information",
      subtitle = subtitle_lab) +
    theme(
      legend.position = .legend_pos,
      legend.title = element_blank(),
      # Control legend text alignment:
      legend.text.align = 0, # 0 left (default), 1 right
      # Remove background and box around the legend:
      legend.background = element_rect(fill = NA, color = NA),
      legend.spacing = unit(0, "cm"), # spacing between legend items
      legend.spacing.y = unit(-0.195, "cm"), # bring legends closer
      legend.margin=margin(t = -8), # remove space between it x-axis
      # Add a box around the keys:
      legend.key = element_rect(fill = "white", colour = "grey"),
      legend.key.size = unit(0.35, "cm"),
      # Add a border and space around the plot:
      panel.border = element_rect(colour = 'black', fill = NA),
      plot.margin = unit(c(0,1,0,0), "cm"), # more space LHS
      # Adjust title size and position:
      #plot.title.position = "plot"
      plot.subtitle = element_text(size = 6, face = "italic"))

  # Show/hide WTP on the CEAF:
  if(.show_wtp) {
    ## CEAF plot willingness-to-pay (WTP) values:
    .wtp = .wtp_threshold %>%
      as_tibble() %>%
      mutate(
        x_cord = .wtp_threshold,
        y_cord = 1,
        angle_cord = 90,
        label_cord = paste0("£", format(.wtp_threshold,
                                        big.mark = ",")),
        lty_ = "Willingness-to-pay (£)")

    ## Plot:
    p <- p +
      geom_vline(
        data = .wtp,
        aes(xintercept = x_cord,
            linetype = lty_),
        colour = "dark gray") +
      scale_linetype_manual(
        breaks = .wtp$lty_[1], # keep one for the legend
        values = rep(3, nrow(.wtp))) +
      guides(
        # Remove the shapes from the line:
        linetype = guide_legend(
          override.aes = list(order = 2,
                              shape = NA, # remove shape
                              color = 'black')))
  }

  # Label WTP value(s) on the CEAF:
  if(.label_wtp) {
    p <- p +
      ggrepel::geom_text_repel(
        data = .wtp,
        aes(x = x_cord,
            y = y_cord,
            angle = angle_cord,
            label = label_cord),
        size = 1.5,
        show.legend = FALSE)
  }

  return(p)
}
