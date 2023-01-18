################################################################################
#
# Script Name:        plot_eNMB_.R
# Module Name:        Economic/PSA
# Script Description: Defines the function that plots the expected Net
#                     Monitory Benefit (eNMB)
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Plot Expected Net Monetary Benefit (eNMB)
#'
#' @param .PSA_data A list of class shinyPSA that contains summary PSA
#' results.
#' @param ... Additional arguments that include:
#' legend position \code{.legend_pos = "bottom"},
#' willingness-to-pay threshold(s) \code{.wtp_threshold = c(20000, 30000)},
#' show WTP threshold(s) lines \code{.show_wtp = TRUE},
#' show WTP threshold(s) labels \code{.label_wtp = TRUE},
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
#' p <- plot_eNMB_(PSA_summary,
#'                .legend_pos = NULL,
#'                .wtp_threshold = c(2000, 10000, 20000, 25000),
#'                .show_wtp = TRUE,
#'                .label_wtp = FALSE,
#'                .zoom = FALSE,
#'                .zoom_cords = NULL,
#'                .show_title = TRUE)
#'
#' p
#' }
#'
plot_eNMB_ <- function(.PSA_data, ...) {
  # Grab the function's environment for correct assignment in assign():
  env_ = environment()
  # Define defaults:
  default_args <- list(
    '.show_title' = TRUE,
    '.legend_pos' = c(0.22, 0.78), # c(x, y) between 0:1 or character
    '.wtp_threshold' = c(20000, 30000),
    '.show_wtp' = TRUE, # TRUE/FALSE
    '.label_wtp' = FALSE, # TRUE/FALSE
    '.zoom' = FALSE, # TRUE/FALSE
    '.zoom_cords' = NULL) # c(x, x)  min and max x axis values
  # Grab additional arguments:
  args_ <- list(...)
  # Assign additional arguments:
  assign_extraArgs_(
    .default_args_ = default_args,
    .args_ = args_,
    .env_ = env_)

  # Plot data:
  enmb_df <- .PSA_data$e.NMB %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      'WTP threshold' = .PSA_data$WTPs,
      'Best option' = .PSA_data$best_name) %>%
    tidyr::pivot_longer(
      cols = colnames(.PSA_data$e.NMB),
      names_to = 'Option',
      values_to = 'eNMB')

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
    ggplot2::coord_cartesian(
      ylim = y_cords,
      xlim = .zoom_cords,
      expand = FALSE) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = 'grey',
      size = 0.1) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = 'grey',
      size = 0.1) +
    ggplot2::geom_line(
      data = enmb_df,
      ggplot2::aes(
        x = `WTP threshold`,
        y = eNMB,
        group = Option,
        color = Option),
      size = 0.4) +
    ggplot2::scale_x_continuous(labels = scales::dollar_format(
      prefix = "\u00A3")) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(
      prefix = "\u00A3")) +
    ggplot2::theme(
      plot.title.position = "plot", # Start title from near the margin
      legend.position = .legend_pos,
      legend.title = ggplot2::element_blank(),
      # Control legend text alignment:
      legend.text.align = 0, # 0 left (default), 1 right
      # Remove background and box around the legend:
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.spacing = ggplot2::unit(0, "cm"), # spacing between legend items
      legend.spacing.y = ggplot2::unit(-0.195, "cm"), # bring legends closer
      #legend.margin=margin(t = -8), # remove space between it x-axis
      # Add a box around the keys:
      legend.key = ggplot2::element_rect(fill = "white", colour = "grey"),
      legend.key.size = ggplot2::unit(0.35, "cm"),
      # Add a border and space around the plot:
      panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
      plot.margin = ggplot2::unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                                  c("points", "cm", "points", "points"))) +
    ggplot2::labs(
      x = "Willingness-to-pay (\u00A3)",
      y = "Expected Net Monetary Benefit (\u00A3)") +
    ggplot2::guides(
      # Increase the size of the points in the legend:
      color = ggplot2::guide_legend(
        override.aes = list(order = 1,
                            size = 3,
                            alpha = 1,
                            stroke = 1)))

  # Show/hide WTP on the eNMB:
  if(.show_wtp) {
    ## eNMB plot willingness-to-pay (WTP) values:
    .wtp = .wtp_threshold %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        x_cord = .wtp_threshold,
        y_cord = max(enmb_df$eNMB),
        angle_cord = 0,
        label_cord = scales::dollar(
          x = .wtp_threshold,
          prefix = "\u00A3"
        ),
        lty_ = "Willingness-to-pay (\u00A3)")

    ## Plot:
    p <- p +
      ggplot2::geom_vline(
        data = .wtp,
        ggplot2::aes(
          xintercept = x_cord,
          linetype = lty_),
        color = 'dark gray') +
      ggplot2::scale_alpha_manual(
        breaks = .wtp$lty_[1], # keep one for the legend
        values = rep(1, nrow(.wtp))) +
      ggplot2::scale_linetype_manual(
        breaks = c("Willingness-to-pay (\u00A3)" = "Willingness-to-pay (\u00A3)"),
        values = c("Willingness-to-pay (\u00A3)" = 3)) +
      ggplot2::guides(
        # Remove the shapes from the line:
        linetype = ggplot2::guide_legend(
          override.aes = list(order = 2,
                              size = 1,
                              shape = NA, # remove shape
                              color = 'black')))
  }

  # Label WTP value(s) on the eNMB:
  if(.label_wtp) {
    p <- p +
      ggrepel::geom_text_repel(
        data = .wtp,
        ggplot2::aes(
          x = x_cord,
          y = y_cord,
          angle = angle_cord,
          label = label_cord),
        size = 1.5,
        show.legend = FALSE)
  }

  # Show/hide plot title:
  if(.show_title) {
    p <- p +
      ggplot2::labs(
        title = "Expected Net Monetary Benefit (eNMB)")
  }

  return(p)
}
