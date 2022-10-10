################################################################################
#
# Script Name:        check_stability.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of functions that aid checking the stability
#                     of PSA mean values.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Check stability of PSA outputs
#' This function produces a set of plots to allow modellers to investigate if
#' the number of PSA runs were sufficient or not!
#'
#' @param .PSA_data A list of class shinyPSA that contains summary PSA
#' results.
#' @param .effs A tibble containing the \code{effects} from PSA. Number of
#' \code{columns} is equal to the interventions while the number of
#' \code{rows} is equal to the number of PSA simulations to be summarised.
#' @param .costs A tibble containing the \code{costs} from PSA. Number of
#' \code{columns} is equal to the interventions while the number of
#' \code{rows} is equal to the number of PSA simulations to be summarised.
#' @param .interventions A vector containing the names of all interventions.
#' If not provided or less names than needed is provided, the function will
#' generate generic names, for example \code{intervention 1}.
#' @param ... Additoinal plot arguments, which currently supports:
#' .legend_pos (legend position): format (x, y) where x and y are between
#' \code{0, 1}. If missing, the function uses default (0.8, 0.85).
#'
#' @return A list with objects of class ggplot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ShinyPSA)
#'
#' p <- check_PSA_stability(
#'          .effs = ShinyPSA::Hypothetical_PSA$e,
#'          .costs = ShinyPSA::Hypothetical_PSA$c,
#'          .interventions = ShinyPSA::Hypothetical_PSA$treats)
#'
#' p[["Effects stability"]]
#' p[["Costs stability"]]
#' p[["Cost per effect stability"]]
#' }
check_PSA_stability <- function(.PSA_data = NULL, .effs = NULL, .costs = NULL,
                                .interventions = NULL, ...) {
  # Grab effects and costs if .PSA_data was supplied but not .effs and .costs:----
  if(is.null(.effs)) {
    .effs = .PSA_data$e
  }
  if(is.null(.costs)) {
    .costs = .PSA_data$c
  }
  if(is.null(.interventions)) {
    .interventions = .PSA_data$interventions
  }

  # Stop if .effs & .costs are not of class tibble or have unequal dims:----
  stopifnot('.effs is not a tibble' = "data.frame" %in% class(.effs),
            '.costs is not a tibble' = "data.frame" %in% class(.costs),
            '.effs and .costs have unequal dimensions' =
              dim(.effs) == dim(.costs))

  # Get number of interventions in supplied matrix:----
  n.comparators <- ncol(.effs) # Number of interventions

  # Check supplied interventions labels, create ones if any is missing:----
  if(is.null(.interventions)) {
    .interventions <- paste("intervention", 1:n.comparators)
  }
  if(length(.interventions) != n.comparators) {
    .interventions <- paste("intervention", 1:n.comparators)
  }

  # Ensure .effs and .costs are tibbles and name columns appropriately:----
  .effs <- .effs %>%
    dplyr::as_tibble(.name_repair = "unique") %>%
    `colnames<-`(.interventions)
  .costs <- .costs %>%
    dplyr::as_tibble(.name_repair = "unique") %>%
    `colnames<-`(.interventions)

  # Estimate PSA outputs' stability:----
  effs_stab <- purrr::map_dfc(
    .x = .effs,
    .f = dplyr::cummean)
  csts_stab <- purrr::map_dfc(
    .x = .costs,
    .f = dplyr::cummean)
  csts_per_effs <- csts_stab / effs_stab

  # Plot data:----
  ## Effects plot data:----
  effs_stab_df <- effs_stab  %>%
    dplyr::mutate(
      `PSA runs` = 1:nrow(.)) %>%
    tidyr::pivot_longer(
      cols = -`PSA runs`,
      names_to = 'Interventions',
      values_to = 'Effects')
  ## Costs plot data:----
  csts_stab_df <- csts_stab %>%
    dplyr::mutate(
      `PSA runs` = 1:nrow(.)) %>%
    tidyr::pivot_longer(
      cols = -`PSA runs`,
      names_to = 'Interventions',
      values_to = 'Costs')
  ## Cost per Effect data:----
  csts_per_effs_df <- csts_per_effs %>%
    dplyr::mutate(
      `PSA runs` = 1:nrow(.)) %>%
    tidyr::pivot_longer(
      cols = -`PSA runs`,
      names_to = 'Interventions',
      values_to = 'Cost (£) per effect')

  # Stability plot defaults:----
  ## Grab the function's environment for correct assignment in assign():----
  env_ = environment()
  ## Define defaults:----
  default_args <- list(
    '.arrange' = "all", # all/v/h
    '.zoom' = FALSE, # TRUE/FALSE
    '.zoom_cords' = NULL, # c(x, x) double min and max x axis values
    '.legend_pos' = c(0.8, 0.85)) # c(x, y) double values between 0:1
  ## Grab additional arguments:----
  args_ <- list(...)
  ## Assign additional arguments:----
  assign_extraArgs_(
    .default_args_ = default_args,
    .args_ = args_,
    .env_ = env_)

  # Stability main plots:----
  plots_list <- NULL
  ## Effects plot:----
  plots_list[['Effects stability']] <- plot_stability_lines(
    df = effs_stab_df,
    x_var = "`PSA runs`",
    y_var = "Effects",
    plot_group = "Interventions",
    title = "Stability of PSA mean effects",
    x_lab = NULL,
    y_lab = "Mean effects",
    .legend_pos_ = .legend_pos,
    .zoom_ = .zoom,
    .zoom_cords_ = .zoom_cords,
    .add_ylabel = FALSE)
  ## Costs plot:----
  plots_list[['Costs stability']] <- plot_stability_lines(
    df = csts_stab_df,
    x_var = "`PSA runs`",
    y_var = "Costs",
    plot_group = "Interventions",
    title = "Stability of PSA mean costs",
    x_lab = NULL,
    y_lab = "Mean costs (\u00A3)",
    .legend_pos_ = .legend_pos,
    .zoom_ = .zoom,
    .zoom_cords_ = .zoom_cords,
    .add_ylabel = TRUE)
  ## Cost per effect stability plot:----
  plots_list[['Cost per effect stability']] <- plot_stability_lines(
    df = csts_per_effs_df,
    x_var = "`PSA runs`",
    y_var = "`Cost (£) per effect`",
    plot_group = "Interventions",
    title = "Stability of PSA mean cost per effect",
    x_lab = "PSA run",
    y_lab = "Mean cost per effect (\u00A3)",
    .legend_pos_ = .legend_pos,
    .zoom_ = .zoom,
    .zoom_cords_ = .zoom_cords,
    .add_ylabel = TRUE)
  ## Combine plots into one compass:----
  if(.arrange == 'all') {
    grouped_plots <- ggpubr::ggarrange(
      plotlist = plots_list[3], # plots_list[c(1, 2)]
      ggpubr::ggarrange(
        plotlist = plots_list[c(1, 2)],
        nrow = 1,
        legend = 'none'),
      nrow = 2,
      legend = "top",
      common.legend = TRUE)
  } else if(.arrange == 'v') {
    grouped_plots <- ggpubr::ggarrange(
      plotlist = plots_list,
      nrow = 1,
      legend = "top",
      common.legend = TRUE)
  } else if (.arrange == 'h') {
    grouped_plots <- ggpubr::ggarrange(
      plotlist = plots_list,
      ncol = 1,
      legend = "bottom",
      common.legend = TRUE)
  }

  return(grouped_plots)
}

# Plot PSA stability graphs
#
# @param df Long format PSA cumulative output dataset
# @param x_var Name of x-axis variable, expects \code{"PSA run"}.
# @param y_var Name of y-axis variable, expects \code{"Mean effects",
# "Mean costs (\u00A3)", "Mean cost per effect (\u00A3)"}.
# @param plot_group Name of grouping variable, expects \code{"Interventions"}.
# @param title Plot title.
# @param x_lab X axis label.
# @param y_lab Y axis label.
# @param .legend_pos_ Legend position based on x and y axis relative location.
# @param .zoom_ Restrict plot to a specific range of PSA runs, if \code{TRUE}.
# @param .zoom_cords_ Range of X axis values \code{c(minimum value,
# maximum value)}
# @param .add_ylabel Add \u00A3 to Y axis if monetary variable is supplied to
# the function.
#
# @return An object of class ggplot2.
#
# @examples
# \dontrun{
# ## Effects plot:
# plots_list[['Effects stability']] <- plot_stability_lines(
#   df = effs_stab_df,
#   x_var = "`PSA runs`",
#   y_var = "Effects",
#   plot_group = "Interventions",
#   title = "Stability of PSA mean effects",
#   x_lab = "PSA run",
#   y_lab = "Mean effects")
# ## Costs plot:
# plots_list[['Costs stability']] <- plot_stability_lines(
#   df = csts_stab_df,
#   x_var = "`PSA runs`",
#   y_var = "Costs",
#   plot_group = "Interventions",
#   title = "Stability of PSA mean costs",
#   x_lab = "PSA run",
#   y_lab = "Mean costs (\u00A3)",
#   .add_ylabel = TRUE)
# ## Cost per effect stability plot:
# plots_list[['Cost per effect stability']] <- plot_stability_lines(
#   df = csts_per_effs_df,
#   x_var = "`PSA runs`",
#   y_var = "`Cost (£) per effect`",
#   plot_group = "Interventions",
#   title = "Stability of PSA mean cost per effect",
#   x_lab = "PSA run",
#   y_lab = "Mean cost per effect (\u00A3)",
#   .add_ylabel = TRUE)
# }
plot_stability_lines <- function(df, x_var, y_var, plot_group, title,
                                 x_lab, y_lab, .legend_pos_ = .legend_pos,
                                 .zoom_ = .zoom,
                                 .zoom_cords_ = .zoom_cords,
                                 .add_ylabel = FALSE) {
  # Zoom:
  if(.zoom_ | (!is.null(.zoom_cords_) & is.numeric(.zoom_cords_))) {
    .zoom_ = TRUE
    if(is.null(.zoom_cords_) |
       (!is.null(.zoom_cords_) & length(.zoom_cords_) != 2))
      .zoom_cords_ = c(0, 1000)
  }

  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(
      ylim = c(0, NA),
      xlim = .zoom_cords_,
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
      data = df,
      ggplot2::aes_string(
        x = x_var,
        y = y_var,
        color = plot_group),
      size = 0.4) +
    ggplot2::theme(
      # Start title from near the margin:
      plot.title.position = "plot",
      # Control legend's position and remove its title:
      legend.position = .legend_pos_,
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
      subtitle = title,
      x = x_lab,
      y = y_lab)

  if(.add_ylabel) {
    p <- p +
      ggplot2::scale_y_continuous(labels = scales::dollar_format(
        prefix = "\u00A3"))
  }

  return(p)
}
