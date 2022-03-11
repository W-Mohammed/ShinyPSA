################################################################################
#
# Script Name:        plot_CEplane_.R
# Module Name:        Economic/PSA
# Script Description: Defines a functions that plots the Cost Effectiveness
#                     Plane.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Plot Cost Effectiveness Plane (CEP).
#'
#' @param .PSA_dt A list of class shinyPSA that contains summary PSA
#' results.
#' @param ... Additional arguments that include:
#' reference intervention \code{.ref = NULL} rescales interventions on CEP,
#' legend position \code{.legend_pos = c(0.8, 0.2)},
#' show ICER information \code{.show_ICER' = TRUE},
#' nudge ICER labels \code{.nudge_labels' = c(NULL, NULL)},
#' willingness-to-pay threshold(s) \code{.wtp_threshold = c(20000, 30000)},
#' show WTP threshold(s) lines and labels \code{.show_wtp = TRUE},
#' seed number to set \code{.seed_no = 1}, and
#' zoom to min/max values \code{.zoom = FALSE}.
#'
#' @return An object of class ggplot
#' @export
#'
#' @examples
#' PSA_summary = summarise_PSA_(
#'   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
#'   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
#'   .interventions = ShinyPSA::Smoking_PSA$treats)
#' library(ShinyPSA)
#' p = plot_CEplane(PSA_summary,
#'                  .ref = 1,
#'                  .show_ICER = F,
#'                  .legend_pos = c(0.8, 0.2),
#'                  .show_wtp = F,
#'                  .zoom = T,
#'                  .wtp_threshold = c(200),
#'                  tst = "PRINT",
#'                  .nudge_labels = c(0.1, -0.1))
#' p
#'
plot_CEplane <- function(.PSA_dt, ...) {
  # Grab function environment for assignment purposes in assign() below:
  .env = environment()
  # Define defaults:
  default_args <- list(
    '.ref' = NULL, # Integer 1:length(interventions)
    '.legend_pos' = c(0.8, 0.2), # c(x, y) double between 0:1
    '.show_ICER' = TRUE, # TRUE/FALSE
    '.nudge_labels' = c(NULL, NULL), # c(x, y) double between 0:1
    '.wtp_threshold' = c(20000, 30000),
    '.show_wtp' = TRUE, # TRUE/FALSE
    '.zoom' = FALSE, # TRUE/FALSE
    '.seed_no' = 1) # Integer
  expected_args <- names(default_args)
  # Grab additional arguments:
  .args_ <- list(...)
  supplied_args <- names(.args_)
  if(any(!supplied_args %in% expected_args))
    message("Argument ",
            paste(supplied_args[!supplied_args %in% expected_args]),
            " is unknown, and therefore ignored")
  # Set additional arguments:
  purrr::walk(
    .x = expected_args,
    .f = function(.arg) {
      assign(.arg,
             if(is.null(.args_[[.arg]])) {
               default_args[[.arg]]
             } else {
               .args_[[.arg]]
             }, envir = .env)
    })

  # Plot data:
  ## CE plot points:
  if(is.null(.ref)) { # No rescaling of point data
    if(.PSA_dt$n.comparators == 2) {
      ce_plane_dt <- .PSA_dt$delta.e %>%
        mutate(sims = row_number()) %>%
        pivot_longer(cols = -sims, names_to = "interventions",
                     values_to = "Effects") %>%
        left_join(x = .,
                  y = .PSA_dt$delta.c %>%
                    mutate(sims = row_number()) %>%
                    pivot_longer(cols = -sims, names_to = "interventions",
                                 values_to = "Costs"),
                  by = c("sims", "interventions"))
    } else {
      ce_plane_dt <- .PSA_dt$e %>%
        mutate(sims = row_number()) %>%
        pivot_longer(cols = -sims, names_to = "interventions",
                     values_to = "Effects") %>%
        left_join(x = .,
                  y = .PSA_dt$c %>%
                    mutate(sims = row_number()) %>%
                    pivot_longer(cols = -sims, names_to = "interventions",
                                 values_to = "Costs"),
                  by = c("sims", "interventions"))
    }
    # Labels:
    .title_lab = "Cost Effectiveness Plane"
    .x_lab = "Effects"
    .y_lab = "Costs"
  } else { # Rescale point data
    ce_plane_dt <- .PSA_dt$e %>%
      calculate_differentials_(.ref = .ref) %>%
      mutate(sims = row_number()) %>%
      pivot_longer(cols = -sims, names_to = "interventions",
                   values_to = "Effects") %>%
      left_join(x = .,
                y = .PSA_dt$c %>%
                  calculate_differentials_(.ref = .ref) %>%
                  mutate(sims = row_number()) %>%
                  pivot_longer(cols = -sims, names_to = "interventions",
                               values_to = "Costs"),
                by = c("sims", "interventions"))
    # Labels:
    .title_lab = "Cost Effectiveness Plane"
    .x_lab = "Effectiveness differential"
    .y_lab = "Cost differential"
  }
  ## CE plot mean values:
  ce_plane_mean_dt <- ce_plane_dt %>%
    group_by(interventions) %>%
    summarise(
      Effects = mean(Effects),
      Costs = mean(Costs))
  ## CE plot x and y axis limits:
  x_lim = c(NA, max(ce_plane_dt$Effects) )
  y_lim = c(NA, max(ce_plane_dt$Costs) )
  ## CE plot ICER labels nudging values:
  .nudge_labels[1] = max(ce_plane_dt$Effects) *
    .nudge_labels[1]
  .nudge_labels[2] = (max(ce_plane_dt$Costs) - min(ce_plane_dt$Costs)) *
    .nudge_labels[2]
  ## CE plot willingness-to-pay (WTP) values:
  .wtp = .wtp_threshold %>%
    as_tibble() %>%
    mutate(x_cord = max(ce_plane_dt$Costs) /
             .wtp_threshold, # get coordinates dynamically
           y_cord = max(ce_plane_dt$Costs), # get coordinates dynamically
           # set angle dynamically by relative rise/run values:
           angle_cord = atan((y_cord/y_cord) / (x_cord)) *
             (180/pi),
           label_cord = paste0("£", format(.wtp_threshold,
                                           big.mark = ",")),
           lty_ = "Willingness-to-pay threshold")

  # Plot:
  p <- ggplot() +
    geom_hline(
      yintercept = 0, colour = "dark gray") +
    geom_vline(
      xintercept = 0, colour = "dark gray") +
    geom_point(
      data = ce_plane_dt,
      aes(x = Effects,
          y = Costs,
          color = interventions),
      size = 1, alpha = 0.5) +
    scale_y_continuous(
      labels = scales::dollar_format(prefix = "£",
                                     big.mark = ",")) +
    geom_point(
      data = ce_plane_mean_dt,
      aes(x = Effects,
          y = Costs,
          fill = interventions),
      shape = 21, colour = "black", show.legend = TRUE,
      size = 2, alpha = 1, stroke = 0.6) +
    ## Keep one value in the legend:
    scale_fill_discrete(
      breaks = ce_plane_mean_dt$interventions[1], # keep one
      labels = "Mean effects & costs") + # change its label
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
      # Add a border around the plot:
      panel.border = element_rect(colour = 'black', fill = NA)) +
    labs(
      title = .title_lab,
      x = .x_lab,
      y = .y_lab) +
    guides(
      # Increase the size of the points in the legend:
      color = guide_legend(
        override.aes = list(size = 1.5,
                            alpha = 1,
                            stroke = NA, # remove stroke
                            linetype = 0)), # remove line
      # Remove the fill colour in shape 21, generalising it to all options:
      fill = guide_legend(
        override.aes = list(size = 2.5,
                            alpha = 1,
                            fill = NA, # remove fill
                            linetype = 0))) # remove line

  # Show/hide ICER label(s) on the CE plot:
  if(.show_ICER) {
    p <- p +
      ggrepel::geom_text_repel(
        data = ce_plane_mean_dt,
        aes(x = Effects,
            y = Costs,
            label = .PSA_dt$ICER$icer_label),
        force_pull = 8,
        size = 2.5,
        point.padding = 0,
        nudge_x = .nudge_labels[1],
        nudge_y = .nudge_labels[2],
        segment.curvature = 1e-8,
        arrow = arrow(length = unit(0.015, "npc")),
        max.overlaps = Inf,
        min.segment.length = 0)
  }

  # Show/hide WTP label(s) on the CE plot:
  if(.show_wtp) {
    p <- p +
      geom_abline(
        data = .wtp,
        aes(intercept = 0,
            slope = value,
            linetype = lty_),
        show.legend = TRUE) +
      scale_linetype_manual(
        breaks = .wtp$lty_[1], # keep one for the legend
        values = rep(3, nrow(.wtp))) +
      ggrepel::geom_text_repel(
        data = .wtp,
        aes(x = x_cord,
            y = y_cord,
            label = label_cord,
            angle = angle_cord),
        size = 2,
        show.legend = FALSE) +
      guides(
        # Remove the stroke from the line:
        linetype = guide_legend(
          override.aes = list(stroke = NA)) # remove stroke
      )
  }

  # Zoom to max x and y values:
  if(.zoom) {
    p <- p +
      coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE)
  }

  return(p)
}
