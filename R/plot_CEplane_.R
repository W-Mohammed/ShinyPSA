################################################################################
#
# Script Name:        plot_CEplane_.R
# Module Name:        Economic/PSA
# Script Description: Defines the function that plots the Cost Effectiveness
#                     Plane.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Plot Cost Effectiveness Plane (CEP).
#'
#' @param .PSA_data A list of class shinyPSA that contains summary PSA
#' results.
#' @param ... Additional arguments that include:
#' reference intervention \code{.ref = NULL} rescales interventions on CEP,
#' legend position \code{.legend_pos = c(0.8, 0.2)},
#' show ICER information \code{.show_ICER' = TRUE},
#' nudge ICER labels \code{.nudge_labels' = c(NULL, NULL)},
#' willingness-to-pay threshold(s) \code{.wtp_threshold = c(20000, 30000)},
#' show WTP threshold(s) lines and labels \code{.show_wtp = TRUE}, and
#' seed number to set \code{.seed_no = 1}.
#'
#' @return An object of class ggplot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ShinyPSA)
#'
#' PSA_summary <- summarise_PSA_(
#'   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
#'   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
#'   .interventions = ShinyPSA::Vaccine_PSA$treats)
#'
#' p <- plot_CEplane_(PSA_summary,
#'                  .ref = 1,
#'                  .show_ICER = TRUE,
#'                  .legend_pos = c(0.8, 0.2),
#'                  .show_wtp = FALSE,
#'                  .zoom = T,
#'                  .wtp_threshold = c(200),
#'                  tst = "PRINT", # this will be ignored
#'                  .nudge_labels = c(0.1, -0.1),
#'                  .zoom_cords = c(-0.001, 0.001, -5, 5)))
#'
#' p
#' }
#'
plot_CEplane_ <- function(.PSA_data, ...) {
  # Grab the function's environment for correct assignment in assign():
  env_ = environment()
  # Define defaults:
  default_args <- list(
    '.ref' = NULL, # Integer 1:length(interventions)
    '.legend_pos' = c(0.8, 0.2), # c(x, y) double between 0:1
    '.show_ICER' = TRUE, # TRUE/FALSE
    '.nudge_labels' = c(NULL, NULL), # c(x, y) double between 0:1
    '.wtp_threshold' = c(20000, 30000),
    '.show_wtp' = TRUE, # TRUE/FALSE
    '.zoom' = FALSE, # TRUE/FALSE
    '.zoom_cords' = NULL) # double c(min x, max x, min y, max y)
  # Grab additional arguments:
  args_ <- list(...)
  # Assign additional arguments:
  ShinyPSA::assign_extraArgs_(.default_args_ = default_args,
                              .args_ = args_,
                              .env_ = env_)

  # Plot data:
  ## CE plot points:
  if(is.null(.ref)) { # No rescaling of point data
    ce_plane_dt <- .PSA_data$e %>%
      dplyr::mutate(sims = dplyr::row_number()) %>%
      tidyr::pivot_longer(
        cols = -sims,
        names_to = "interventions",
        values_to = "Effects") %>%
      dplyr::left_join(
        x = .,
        y = .PSA_data$c %>%
          dplyr::mutate(sims = dplyr::row_number()) %>%
          tidyr::pivot_longer(cols = -sims,
                              names_to = "interventions",
                              values_to = "Costs"),
        by = c("sims", "interventions"))
    # Labels:
    .title_lab = "Cost Effectiveness Plane"
    .x_lab = "Effects"
    .y_lab = "Costs (\u00A3)"
  } else { # Rescale point data
    ce_plane_dt <- .PSA_data$e %>%
      ShinyPSA::calculate_differentials_(.ref = .ref) %>%
      dplyr::mutate(sims = dplyr::row_number()) %>%
      tidyr::pivot_longer(
        cols = -sims,
        names_to = "interventions",
        values_to = "Effects") %>%
      dplyr::left_join(
        x = .,
        y = .PSA_data$c %>%
          ShinyPSA::calculate_differentials_(.ref = .ref) %>%
          dplyr::mutate(sims = dplyr::row_number()) %>%
          tidyr::pivot_longer(cols = -sims,
                              names_to = "interventions",
                              values_to = "Costs"),
        by = c("sims", "interventions"))
    # Labels:
    .title_lab = "Cost Effectiveness Plane"
    .x_lab = "Effectiveness differential"
    .y_lab = "Cost differential (\u00A3)"
  }

  ## CE plot mean values:
  ce_plane_mean_dt <- ce_plane_dt %>%
    dplyr::group_by(interventions) %>%
    dplyr::summarise(
      Effects = mean(Effects),
      Costs = mean(Costs)) %>%
    dplyr::left_join(
      x = .,
      y = .PSA_data$ICER %>%
        select(intervention, icer_label) %>%
        rename("interventions" = intervention),
      by = "interventions") %>%
    dplyr::rename("Label" = icer_label)

  # Plot:
  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = 0, colour = "dark gray") +
    ggplot2::geom_vline(
      xintercept = 0, colour = "dark gray") +
    ggplot2::geom_point(
      data = ce_plane_dt,
      ggplot2::aes(
        x = Effects,
        y = Costs,
        color = interventions),
      size = 1, alpha = 0.5) +
    ggplot2::scale_y_continuous(
      labels = scales::dollar_format(prefix = "\u00A3",
                                     big.mark = ",")) +
    ggplot2::geom_point(
      data = ce_plane_mean_dt,
      ggplot2::aes(
        x = Effects,
        y = Costs,
        fill = interventions),
      shape = 21, colour = "black", show.legend = TRUE,
      size = 2, alpha = 1, stroke = 0.6) +
    ## Keep one value in the legend:
    ggplot2::scale_fill_discrete(
      breaks = ce_plane_mean_dt$interventions[1], # keep one
      labels = "Mean effects/costs") + # change its label
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
      # Add a border around the plot:
      panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
      plot.margin = ggplot2::unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                                  c("points", "cm", "points", "points"))) +
    ggplot2::labs(
      title = .title_lab,
      x = .x_lab,
      y = .y_lab) +
    ggplot2::guides(
      # Increase the size of the points in the legend:
      color = ggplot2::guide_legend(
        override.aes = list(order = 1,
                            size = 1.5,
                            alpha = 1,
                            stroke = NA, # remove stroke
                            linetype = 0)), # remove line
      # Remove the fill colour in shape 21, generalising it to all options:
      fill = ggplot2::guide_legend(
        override.aes = list(order = 2,
                            size = 2.5,
                            alpha = 1,
                            fill = NA, # remove fill
                            linetype = 0))) # remove line

  # Show/hide ICER label(s) on the CE plot:
  if(.show_ICER) {
    ## CE plot ICER labels nudging values:
    .nudge_labels[1] = max(ce_plane_dt$Effects) *
      .nudge_labels[1]
    .nudge_labels[2] = (max(ce_plane_dt$Costs) - min(ce_plane_dt$Costs)) *
      .nudge_labels[2]

    ## Plot:
    p <- p +
      ggrepel::geom_text_repel(
        data = ce_plane_mean_dt,
        ggplot2::aes(
          x = Effects,
          y = Costs,
          label = Label),
        force_pull = 8,
        size = 2.5,
        point.padding = 0,
        nudge_x = .nudge_labels[1],
        nudge_y = .nudge_labels[2],
        segment.curvature = 1e-8,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.015, "npc")),
        max.overlaps = Inf,
        min.segment.length = 0)
  }

  # Show/hide WTP label(s) on the CE plot:
  if(.show_wtp) {
    ## CE plot willingness-to-pay (WTP) values:
    if(max(ce_plane_dt$Effects) < max(abs(ce_plane_dt$Effects))){
      ### Get labels' coordinates dynamically:
      x_cord = ifelse(rep(min(ce_plane_dt$Costs), length(.wtp_threshold)) <
                        0,
                      ifelse((min(ce_plane_dt$Costs) / .wtp_threshold) <=
                               min(ce_plane_dt$Effects),
                             min(ce_plane_dt$Effects),
                             (min(ce_plane_dt$Costs) / .wtp_threshold)),
                      ifelse(-(min(ce_plane_dt$Costs) / .wtp_threshold) <=
                               min(ce_plane_dt$Effects),
                             min(ce_plane_dt$Effects),
                             -(min(ce_plane_dt$Costs) / .wtp_threshold)))
      y_cord = ifelse(rep(max(ce_plane_dt$Costs), length(.wtp_threshold)) <
                        0,
                      0,
                      ifelse((min(ce_plane_dt$Effects) * .wtp_threshold) <=
                               min(ce_plane_dt$Costs),
                             min(ce_plane_dt$Costs),
                             (min(ce_plane_dt$Effects) * .wtp_threshold)))
    } else {
      ### Get labels' coordinates dynamically:
      x_cord = ifelse((max(ce_plane_dt$Costs) / .wtp_threshold) >=
                        max(ce_plane_dt$Effects),
                      max(ce_plane_dt$Effects),
                      (max(ce_plane_dt$Costs) / .wtp_threshold))
      y_cord = ifelse(rep(max(ce_plane_dt$Costs), length(.wtp_threshold)) <
                        0,
                      0,
                      ifelse((max(ce_plane_dt$Effects) * .wtp_threshold) >=
                               max(ce_plane_dt$Costs),
                             max(ce_plane_dt$Costs),
                             (max(ce_plane_dt$Effects) * .wtp_threshold)))
    }

    ### Get axis scale to correctly set the labels:
    x_range <- ggplot2::layer_scales(p)$x$range$range
    y_range <- ggplot2::layer_scales(p)$y$range$range
    x_to_y <- (x_range[2] - x_range[1])/(y_range[2] - y_range[1])
    ### Calculate angles:
    angle_cord <- atan(.wtp_threshold * x_to_y) * 180/pi
    ### Put .wtp data on tibble:
    .wtp = .wtp_threshold %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        x_cord = x_cord,
        y_cord = y_cord,
        angle_cord = angle_cord,
        label_cord = scales::dollar(
          x = .wtp_threshold,
          prefix = "\u00A3"
        ),
        lty_ = "Willingness-to-pay (\u00A3)")

    ## Plot:
    p <- p +
      ggplot2::geom_abline(
        data = .wtp,
        ggplot2::aes(intercept = 0,
                     slope = value,
                     linetype = lty_),
        show.legend = TRUE) +
      ggplot2::scale_linetype_manual(
        breaks = .wtp$lty_[1], # keep one for the legend
        values = rep(3, nrow(.wtp))) +
      ggrepel::geom_text_repel(
        data = .wtp,
        ggplot2::aes(x = x_cord,
                     y = y_cord,
                     #angle = angle_cord,
                     label = label_cord),
        size = 1.5,
        show.legend = FALSE) +
      ggplot2::guides(
        # Remove the stroke from the line:
        linetype = ggplot2::guide_legend(
          override.aes = list(order = 3,
                              stroke = NA)) # remove stroke
      )
  }

  # Zoom to max x and y values:
  if(.zoom &
     (is.null(.zoom_cords) |
      if(!is.null(.zoom_cords)) length(.zoom_cords) < 4 else TRUE)) {

    ## CE plot x and y axis limits:
    x_lim = c(NA, max(ce_plane_dt$Effects))
    y_lim = c(NA, max(ce_plane_dt$Costs))

    # Plot:
    p <- p +
      ggplot2::coord_cartesian(xlim = x_lim, ylim = y_lim, expand = !.zoom,
                               default = .zoom)
  }

  if(.zoom & !is.null(.zoom_cords) &
     if(!is.null(.zoom_cords)) length(.zoom_cords) == 4 else FALSE) {
    ## CE plot x and y axis limits:
    x_lim = c(.zoom_cords[1], .zoom_cords[2])
    y_lim = c(.zoom_cords[3], .zoom_cords[4])

    # Plot:
    p <- p +
      ggplot2::coord_cartesian(xlim = x_lim, ylim = y_lim, expand = !.zoom,
                               default = .zoom)
  }

  return(p)
}
