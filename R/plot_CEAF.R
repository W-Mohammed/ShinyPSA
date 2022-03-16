################################################################################
#
# Script Name:        plot_CEAF_.R
# Module Name:        Economic/PSA
# Script Description: Defines a functions that plots the Cost Effectiveness
#                     Acceptability Frontier (CEAF)
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Plot Cost Effectiveness Acceptability Frontier (CEAF)
#'
#' @param .PSA_data A list of class shinyPSA that contains summary PSA
#' results.
#' @param ...Additional arguments that include:
#' legend position \code{.legend_pos = c(0.8, 0.85)},
#' willingness-to-pay threshold(s) \code{.wtp_threshold = c(20000, 30000)},
#' show WTP threshold(s) lines \code{.show_wtp = TRUE},
#' show WTP threshold(s) labels \code{.label_wtp' = TRUE},
#' zoom to min/max values \code{.zoom = FALSE},
#' zoom to supplied coordinates values \code{.zoom_cords = NULL}, and
#' show 20 points/shapes along the lines \code{.show_shapes = FALSE}.
#'
#' @return
#' @export
#'
#' @examples
#' #' library(ShinyPSA)
#' PSA_summary = summarise_PSA_(
#'   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
#'   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
#'   .interventions = ShinyPSA::Smoking_PSA$treats)
#' p = plot_CEAF_(PSA_summary,
#'                .legend_pos = NULL,
#'                .wtp_threshold = c(2000, 10000, 20000, 25000),
#'                .show_wtp = TRUE,
#'                .label_wtp = FALSE,
#'                .zoom = FALSE,
#'                .zoom_cords = NULL,
#'                .show_shapes = TRUE)
#' p
#'
plot_CEAF_ <- function(.PSA_data, ...) {
  # Grab the function's environment for correct assignment in assign():
  env_ = environment()
  # Define defaults:
  default_args <- list(
    '.legend_pos' = c(0.8, 0.85), # c(x, y) double between 0:1
    '.wtp_threshold' = c(20000, 30000),
    '.show_wtp' = TRUE, # TRUE/FALSE
    '.label_wtp' = FALSE, # TRUE/FALSE
    '.zoom' = FALSE, # TRUE/FALSE
    '.zoom_cords' = NULL, # c(x, x) double min and max x axis values
    '.show_shapes' = FALSE) # TRUE/FALSE
  # Grab additional arguments:
  args_ <- list(...)
  # Assign additional arguments:
  assign_extraArgs_(.default_args_ = default_args,
                    .args_ = args_,
                    .env_ = env_)

  # Plot data:
  ceaf_df = .PSA_data$CEAF %>%
    mutate('Best option' = .PSA_data$best_name,
           'WTP threshold' = .PSA_data$WTPs)

  # Zoom:
  if(.zoom | (!is.null(.zoom_cords) & is.numeric(.zoom_cords))) {
    .zoom = TRUE
    if(is.null(.zoom_cords) |
       (!is.null(.zoom_cords) & length(.zoom_cords) != 2))
      .zoom_cords = c(0, 31000)
  }

  # CEAC main plot:
  p <- ggplot() +
    coord_cartesian(ylim = c(0, 1), xlim = .zoom_cords, expand = FALSE) +
    geom_hline(
      yintercept = 0,
      color = 'grey',
      size = 0.1) +
    geom_vline(
      xintercept = 0,
      color = 'grey',
      size = 0.1) +
    geom_line(
      data = ceaf_df,
      aes(x = `WTP threshold`,
          y = ceaf,
          color = `Best option`),
      size = 0.4) +
    scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(
      legend.position = .legend_pos,
      legend.title = element_blank(),
      # Control legend text alignment:
      legend.text.align = 0, # 0 left (default), 1 right
      # Remove background and box around the legend:
      legend.background = element_rect(fill = NA, color = NA),
      legend.spacing = unit(0, "cm"), # spacing between legend items
      legend.spacing.y = unit(-0.195, "cm"), # bring legends closer
      # Add a box around the keys:
      legend.key = element_rect(fill = "white", colour = "grey"),
      legend.key.size = unit(0.35, "cm"),
      # Add a border and space around the plot:
      panel.border = element_rect(colour = 'black', fill = NA),
      plot.margin = unit(c(0,1,0,0), "cm")) + # more space LHS
    labs(
      title = "Cost-effectiveness acceptability frontier (CEAF)",
      x = "Willingness-to-pay",
      y = "Probability cost-effective") +
    guides(
      # Increase the size of the points in the legend:
      color = guide_legend(
        override.aes = list(size = 1,
                            alpha = 1,
                            shape = NA)))

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
        lty_ = "Willingness-to-pay threshold")

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
          override.aes = list(shape = NA, # remove shape
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

  # Show/hide shapes on the CEAF:
  if(.show_shapes) {
    ## Data:
    ### Select a few points:
    n_points <- .PSA_data$WTPstar
    n_points <- c(n_points,
                  seq(from = 0,
                      to = .PSA_data$WTPs[length(.PSA_data$WTPs)],
                      length.out = 20),
                  .PSA_data$WTPs[length(.PSA_data$WTPs)])
    n_points <- sort(
      unique(
        plyr::round_any(n_points, 100, f = ceiling)))

    ## Plot:
    p <- p +
      geom_point(
        data = ceaf_df %>%
          filter(`WTP threshold` %in% n_points),
        aes(x = `WTP threshold`,
            y = ceaf,
            color = `Best option`,
            shape = `Best option`),
        size = 1.5,
        alpha = 0.8,
        show.legend = TRUE)
  }

  return(p)
}
