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
#' show WTP threshold(s) labels \code{.label_wtp' = TRUE},
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
#'                .zoom_cords = NULL)
#'
#' p
#' }
#'
plot_eNMB_ <- function(.PSA_data, ...) {
  # Grab the function's environment for correct assignment in assign():
  env_ = environment()
  # Define defaults:
  default_args <- list(
    '.legend_pos' = c(0.22, 0.78), # c(x, y) between 0:1 or character
    '.wtp_threshold' = c(20000, 30000),
    '.show_wtp' = TRUE, # TRUE/FALSE
    '.label_wtp' = FALSE, # TRUE/FALSE
    '.zoom' = FALSE, # TRUE/FALSE
    '.zoom_cords' = NULL) # c(x, x)  min and max x axis values
  # Grab additional arguments:
  args_ <- list(...)
  # Assign additional arguments:
  assign_extraArgs_(.default_args_ = default_args,
                    .args_ = args_,
                    .env_ = env_)

  # Plot data:
  enmb_df <- .PSA_data$e.NMB %>%
    as_tibble() %>%
    mutate('WTP threshold' = .PSA_data$WTPs,
           'Best option' = .PSA_data$best_name) %>%
    pivot_longer(cols = colnames(.PSA_data$e.NMB),
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
      data = enmb_df,
      aes(x = `WTP threshold`,
          y = eNMB,
          group = Option,
          linetype = Option,
          color = Option),
      size = 0.4) +
    scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
    theme(
      plot.title.position = "plot", # Start title from near the margin
      legend.position = .legend_pos,
      legend.title = element_blank(),
      # Control legend text alignment:
      legend.text.align = 0, # 0 left (default), 1 right
      # Remove background and box around the legend:
      legend.background = element_rect(fill = NA, color = NA),
      legend.spacing = unit(0, "cm"), # spacing between legend items
      legend.spacing.y = unit(-0.195, "cm"), # bring legends closer
      #legend.margin=margin(t = -8), # remove space between it x-axis
      # Add a box around the keys:
      legend.key = element_rect(fill = "white", colour = "grey"),
      legend.key.size = unit(0.35, "cm"),
      # Add a border and space around the plot:
      panel.border = element_rect(colour = 'black', fill = NA),
      plot.margin = unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                         c("points", "cm", "points", "points"))) +
    labs(
      title = "Expected Net Monetary Benefit (eNMB)",
      x = "Willingness-to-pay (£)",
      y = "Expected Net Monetary Benefit (£)")


  # Show/hide WTP on the CEAF:
  if(.show_wtp) {
    ## CEAF plot willingness-to-pay (WTP) values:
    .wtp = .wtp_threshold %>%
      as_tibble() %>%
      mutate(
        x_cord = .wtp_threshold,
        y_cord = max(enmb_df$eNMB),
        angle_cord = 0,
        label_cord = paste0("£", format(.wtp_threshold,
                                        big.mark = ",")),
        lty_ = "Willingness-to-pay (£)")

    ## Plot:
    p <- p +
      geom_vline(
        data = .wtp,
        aes(xintercept = x_cord,
            alpha = lty_),
        color = 'dark gray',
        linetype = 3) +
      scale_alpha_manual(
        breaks = .wtp$lty_[1], # keep one for the legend
        values = rep(1, nrow(.wtp))) +
      # scale_colour_manual(
      #   breaks = .wtp$lty_[1], # keep one for the legend
      #   values = rep("dark gray", nrow(.wtp)))
    guides(
        # Remove the shapes from the line:
        alpha = guide_legend(
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
