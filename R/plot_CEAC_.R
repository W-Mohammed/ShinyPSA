################################################################################
#
# Script Name:        plot_CEAC_.R
# Module Name:        Economic/PSA
# Script Description: Defines the functions that plots the Cost Effectiveness
#                     Acceptability Curve (CEAC)
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Plot Cost Effectiveness Acceptability Curve (CEAC)
#'
#' @param .PSA_data A list of class shinyPSA that contains summary PSA
#' results.
#' @param ... Additional arguments that include:
#' reference intervention \code{.ref = NULL} rescales interventions on CEP,
#' legend position \code{.legend_pos = c(0.8, 0.85)},
#' willingness-to-pay threshold(s) \code{.wtp_threshold = c(20000, 30000)},
#' show WTP threshold(s) lines \code{.show_wtp = TRUE},
#' show WTP threshold(s) labels \code{.label_wtp = TRUE},
#' zoom to min/max values \code{.zoom = FALSE},
#' zoom to supplied coordinates values \code{.zoom_cords = NULL},
#' show 20 points/shapes along the lines \code{.show_shapes = FALSE}, and
#' add Cost Effectiveness Acceptability Curve \code{.add_CEAF = FALSE}.
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
#' p <- plot_CEAC_(PSA_summary,
#'                .ref = 1,
#'                .legend_pos = NULL,
#'                .wtp_threshold = c(2000, 10000, 20000, 25000),
#'                .show_wtp = TRUE,
#'                .label_wtp = FALSE,
#'                .zoom = FALSE,
#'                .zoom_cords = NULL,
#'                .show_shapes = TRUE,
#'                .add_CEAF = TRUE)
#'
#' p
#' }
plot_CEAC_ <- function(.PSA_data, ...) {
  # Grab the function's environment for correct assignment in assign():
  env_ = environment()
  # Define defaults:
  default_args <- list(
    '.ref' = NULL, # Integer 1:length(interventions)
    '.legend_pos' = c(0.8, 0.85), # c(x, y) double between 0:1
    '.wtp_threshold' = c(20000, 30000),
    '.show_wtp' = TRUE, # TRUE/FALSE
    '.label_wtp' = FALSE, # TRUE/FALSE
    '.zoom' = FALSE, # TRUE/FALSE
    '.zoom_cords' = NULL, # c(x, x) double min and max x axis values
    '.show_shapes' = FALSE, # TRUE/FALSE
    '.add_CEAF' = FALSE) # TRUE/FALSE
  # Grab additional arguments:
  args_ <- list(...)
  # Assign additional arguments:
  assign_extraArgs_(
    .default_args_ = default_args,
    .args_ = args_,
    .env_ = env_)
  # Override .ref if more than two interventions exist:
  if(!is.null(.ref) & (length(.PSA_data$interventions) > 2)) .ref = NULL
  # Function to remove intervention from plot data:
  drop_intervention <- function(.data_, .ref = .ref) {
    if(!is.null(.ref)) .data_ <- .data_ %>%
        dplyr::select(-dplyr::all_of(.ref))
    else .data_ <- .data_
    return(.data_)
  }

  # Plot data:
  ceac_df = .PSA_data$CEAC %>%
    drop_intervention(.data_ = ., .ref = .ref) %>%
    dplyr::mutate('WTP threshold' = .PSA_data$WTPs) %>%
    tidyr::pivot_longer(
      cols = -`WTP threshold`,
      names_to = 'Option',
      values_to = 'Probability cost-effective')

  # Zoom:
  if(.zoom | (!is.null(.zoom_cords) & is.numeric(.zoom_cords))) {
    .zoom = TRUE
    if(is.null(.zoom_cords) |
       (!is.null(.zoom_cords) & length(.zoom_cords) != 2))
      .zoom_cords = c(0, 31000)
  }

  # CEAC main plot:
  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(ylim = c(0, 1), xlim = .zoom_cords, expand = FALSE) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = 'grey',
      size = 0.1) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = 'grey',
      size = 0.1) +
    ggplot2::geom_line(
      data = ceac_df,
      ggplot2::aes(
        x = `WTP threshold`,
        y = `Probability cost-effective`,
        color = Option),
      size = 0.4) +
    ggplot2::scale_x_continuous(labels = scales::dollar_format(
      prefix = "\u00A3")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
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
      # Add a box around the keys:
      legend.key = ggplot2::element_rect(fill = "white", colour = "grey"),
      legend.key.size = ggplot2::unit(0.35, "cm"),
      # Add a border and space around the plot:
      panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
      plot.margin = ggplot2::unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                                  c("points", "cm", "points", "points"))) +
    ggplot2::labs(
      title = "Cost Effectiveness Acceptability Curve (CEAC)",
      x = "Willingness-to-pay (\u00A3)",
      y = "Probability cost-effective") +
    ggplot2::guides(
      # Increase the size of the points in the legend:
      color = ggplot2::guide_legend(
        override.aes = list(order = 1,
                            size = 1,
                            alpha = 1,
                            shape = NA)))

  # Show/hide WTP on the CEAC:
  if(.show_wtp) {
    ## CEAC plot willingness-to-pay (WTP) values:
    .wtp = .wtp_threshold %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        x_cord = .wtp_threshold,
        y_cord = 1,
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
        colour = "dark gray") +
      ggplot2::scale_linetype_manual(
        breaks = .wtp$lty_[1], # keep one for the legend
        values = rep(3, nrow(.wtp))) +
      ggplot2::guides(
        # Remove the shapes from the line:
        linetype = ggplot2::guide_legend(
          override.aes = list(order = 3,
                              shape = NA, # remove shape
                              color = 'black')))
  }

  # Label WTP value(s) on the CEAC:
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

  # Show/hide shapes on the CEAC:
  if(.show_shapes) {
    ## Data:
    n_points <- .PSA_data$WTPstar
    n_points <- c(0, n_points,
                  seq(from = 0,
                      to = .PSA_data$WTPs[length(.PSA_data$WTPs)],
                      length.out = 20),
                  .PSA_data$WTPs[length(.PSA_data$WTPs)],
                  .wtp_threshold)
    n_points <- sort(
      unique(
        plyr::round_any(n_points, 100, f = ceiling)))

    ## Plot:
    p <- p +
      ggplot2::geom_point(
        data = ceac_df %>%
          dplyr::filter(`WTP threshold` %in% n_points),
        ggplot2::aes(
          x = `WTP threshold`,
          y = `Probability cost-effective`,
          shape = Option, color = Option),
        size = 1,
        show.legend = TRUE)
  }

  # Show/hide CEAF on the CEAC:
  if(.add_CEAF & (length(.PSA_data$interventions) > 2)) {
    ## Data:
    ### Select a few points:
    n_points <- .PSA_data$WTPstar
    n_points <- c(0, n_points,
                  seq(from = 0,
                      to = .PSA_data$WTPs[length(.PSA_data$WTPs)],
                      length.out = 20),
                  .PSA_data$WTPs[length(.PSA_data$WTPs)],
                  .wtp_threshold)
    n_points <- sort(
      unique(
        plyr::round_any(n_points, 100, f = ceiling)))

    ### CEAF:
    ceaf_df = .PSA_data$CEAF %>%
      dplyr::mutate('Best option' =
                      colnames(.PSA_data$CEAC)[max.col(.PSA_data$CEAC)],
                    'WTP threshold' = .PSA_data$WTPs)

    ## Plot:
    p <- p +
      ggplot2::geom_point(
        data = ceaf_df %>%
          dplyr::filter(`WTP threshold` %in% n_points),
        ggplot2::aes(
          x = `WTP threshold`,
          y = ceaf),
        size = 2,
        stroke = 1,
        alpha = 0.8,
        shape = 21,
        color = "black",
        show.legend = TRUE) +
      ggplot2::scale_fill_manual(
        values = c("CEAF" = "black")) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          override.aes = list(order = 2,
                              shape = 21,
                              size = 2.5,
                              stroke = 1)))
  }

  return(p)
}
