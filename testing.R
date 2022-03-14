# Load library:
pacman::p_load(tidyverse, devtools, BCEA)
load_all()
#PSA_data <- ShinyPSA:::Smoking_PSA
#PSA_data <- ShinyPSA:::Vaccine_PSA
#################################################################
tst = compute_NMBs_(.effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
                    .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
                    .interventions = ShinyPSA::Vaccine_PSA$treats)
tst2 = compute_NMBs_(.effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
                     .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
                     .interventions = ShinyPSA::Vaccine_PSA$treats)
tst_CEAC = compute_CEACs_(.nmb = NULL,
                          .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
                          .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
                          .interventions = ShinyPSA::Vaccine_PSA$treats)
tst_CEAF = compute_CEAFs_(.ceac = tst_CEAC)
tst_ICER = compute_ICERs_(.icer_data = NULL,
                          .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
                          .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
                          .interventions = ShinyPSA::Vaccine_PSA$treat)
tst_CEAC2 = compute_CEACs_(.nmb = NULL,
                           .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
                           .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
                           .interventions = ShinyPSA::Smoking_PSA$treats)
tst_CEAF2 = compute_CEAFs_(.ceac = tst_CEAC2)
tst_ICER2 = compute_ICERs_(.icer_data = NULL,
                           .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
                           .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
                           .interventions = ShinyPSA::Smoking_PSA$treats)
tst_all1 = summarise_PSA_(.effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
                          .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
                          .interventions = ShinyPSA::Vaccine_PSA$treats)
tst_all2 = summarise_PSA_(.effs = as_tibble(ShinyPSA::Smoking_PSA$e),
                          .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
                          .interventions = ShinyPSA::Smoking_PSA$treats)
tst_all1_BCEA = BCEA::bcea(eff = (ShinyPSA::Vaccine_PSA$e),
                           cost = (ShinyPSA::Vaccine_PSA$c),
                           interventions = ShinyPSA::Vaccine_PSA$treats,plot = T)
tst_all2_BCEA = BCEA::bcea(eff = (ShinyPSA::Smoking_PSA$e),
                           cost = (ShinyPSA::Smoking_PSA$c),
                           interventions = ShinyPSA::Smoking_PSA$treats)
compare(tst_all1$EVPI, tst_all1_BCEA$evi, check.attributes = F)
compare(tst_all1$e.NMB$`2: Vaccination`, -tst_all1_BCEA$eib, check.attributes = F)
compare(tst_all1$CEAC$`2: Vaccination`, 1 - tst_all1_BCEA$ceac, check.attributes = F)

compare(tst_all2$EVPI, tst_all2_BCEA$evi, check.attributes = F)

#################################################################

PSA_dt <- summarise_PSA_(.effs = PSA_data$e, .costs = PSA_data$c,
                         .interventions = PSA_data$treats)

# Plots:
ce_plane_dt = PSA_dt$e %>%
  mutate(sims = row_number()) %>%
  pivot_longer(cols = -sims, names_to = "interventions",
               values_to = "effects") %>%
  left_join(x = .,
            y = PSA_dt$c %>%
              mutate(sims = row_number()) %>%
              pivot_longer(cols = -sims, names_to = "interventions",
                           values_to = "costs"),
            by = c("sims", "interventions"))

x_lim = c(NA, max(ce_plane_dt$effects) * 1.1)
y_lim = c(min(ce_plane_dt$costs) * 0.9, max(ce_plane_dt$costs) * 1.1)
wtp = 100
wtp = wtp %>%
  as_tibble() %>%
  mutate(label = paste0("WTP threshold = £", wtp))

set.seed(4)
ce_plane_plot <- ggplot() +
  geom_hline(yintercept = 0, colour = "dark gray") +
  geom_vline(xintercept = 0, colour = "dark gray") +
  geom_point(data = ce_plane_dt,
             aes(x = effects, y = costs, color = interventions),
             size = 1, alpha = 0.5) +
  geom_point(data = PSA_dt$ICER, inherit.aes = FALSE,
             aes(x = qalys, y = costs, fill = intervention),
             shape = 21, colour = "black", show.legend = TRUE,
             size = 1, alpha = 1, stroke = 1) +
  # Keep one value in the legend:
  scale_fill_discrete(
    breaks = PSA_dt$ICER$intervention[1], # keep one
    labels = "Mean effects & costs") + # change its label
  #coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
  ggrepel::geom_text_repel(data = PSA_dt$ICER,
                           aes(x = qalys, y = costs, label = icer_label),
                           force_pull = 8,
                           size = 2.75,
                           point.padding = 0.2,
                           nudge_x = 0.35,
                           nudge_y = 10,
                           segment.curvature = 1e-8,
                           arrow = arrow(length = unit(0.015, "npc")),
                           max.overlaps = Inf,
                           min.segment.length = 0) +
  geom_abline(data = as_tibble(wtp),
              aes(intercept = 0, slope = value, linetype = label),
              show.legend = TRUE) +
  scale_linetype_manual(values = 3) + # 6: dashed 4: dot dashed 3: dots
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8, 0.2),
    #legend.position = 'bottom',
    #legend.box.margin = margin(),
    #legend.box.spacing = unit(0.5, "cm"),
    legend.key = element_rect(fill = "white", colour = "grey"),
    #legend.justification = c("right", "top"),
    # Control legend text alignment:
    legend.text.align = 0, # 0 left (default), 1 right
    # Add a black box around the legend:
    #legend.background = element_rect(fill = NA, color = 'black'),
    legend.background = element_rect(fill = NA),
    legend.spacing = unit(0, "cm"), # spacing between legend items
    legend.spacing.y = unit(-0.195, "cm"), # bring legends closer
    legend.key.size = unit(0.35, "cm"),
    #legend.direction = "horizontal",
    #panel.grid = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'black', fill = NA)) +
  labs(title = "Cost-effectiveness plane",
       x = "Effects",
       y = "Costs") +
  guides(
    # Increase the size of the points in the legend:
    color = guide_legend(override.aes = list(size = 1.5,
                                             alpha = 1,
                                             stroke = NA, # remove storke
                                             linetype = 0)), # remove line
    # Remove the fill colour in shape 21, generalising it to all options:
    fill = guide_legend(override.aes = list(size = 1.5,
                                            alpha = 1,
                                            fill = NA, # remove fill
                                            #stroke = 1,
                                            linetype = 0)), # remove line
    # Remove the storke from the line and adjust the size:
    linetype = guide_legend(override.aes = list(#size = 0.7,
      stroke = NA)))
ce_plane_plot

#########################################################
# CEAC:
ceac_df = PSA_dt$CEAC %>%
  mutate('WTP threshold' = PSA_dt$k) %>%
  pivot_longer(cols = -`WTP threshold`,
               names_to = 'Option',
               values_to = 'Probability cost-effective')

ceaf_df = PSA_dt$CEAF$ceaf %>%
  as_tibble() %>%
  mutate('WTP threshold' = PSA_dt$k) %>%
  rename('CEAF' = value)

ceac_plot = ggplot() +
  geom_line(data = ceaf_df,
            aes(x = `WTP threshold`,
                y = CEAF),
            size = 2, alpha = 0.6, color = 'sky blue',
            show.legend = TRUE) +
  scale_fill_manual(values = c("CEAF" = "gray")) +
  geom_line(data = ceac_df,
            aes(x = `WTP threshold`,
                y = `Probability cost-effective`,
                group = Option,
                #linetype = Option,
                color = Option),
            size = 0.1,
            show.legend = FALSE) +
  geom_point(data = ceac_df,
             aes(x = `WTP threshold`,
                 y = `Probability cost-effective`,
                 shape = Option,
                 color = Option),
             size = 1) +
  coord_cartesian(xlim = c(0, 10000), expand = FALSE) +
  scale_x_continuous(labels = dollar_format(prefix = "£", )) +
  scale_y_continuous(labels = percent_format()) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8, 0.85),
    legend.text.align = 0, # 0 left (default), 1 right
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = "white", colour = "grey"),
    # legend.spacing = unit(0, "cm"), # spacing between legend items
    legend.spacing.y = unit(-0.2, "cm"), # bring legends closer
    legend.key.size = unit(0.2, "cm"),
    # legend.position = 'bottom',
    # legend.box.margin = margin(),
    # legend.direction = "horizontal",
    #panel.grid.major = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'black', fill = NA)) +
  labs(title = "Cost-effectiveness acceptability curve (CEAC)",
       x = "Willingness-to-pay",
       y = "Probability cost-effective") +
  guides(
    # Increase the size of the points in the legend:
    shape = guide_legend(override.aes = list(size = 2,
                                             #alpha = 1,
                                             #stroke = NA, # remove storke
                                             linetype = 0)),
    # line = guide_legend(override.aes = list(size = 0.2)),
    fill = guide_legend(override.aes = list(alpha = 1))
  )

ceac_plot

# bcea(eff = e, cost = c,interventions = treats, plot = T)


ceac_plot = ggplot() +
  geom_line(data = ceac_df,
            aes(x = `WTP threshold`,
                y = `Probability cost-effective`,
                #group = Option,
                color = Option),
            size = 0.4) +
  geom_point(data = ceaf_df,
             aes(x = `WTP threshold`,
                 y = CEAF),
             size = 2, alpha = 0.8, shape = 21, color = "black",
             show.legend = TRUE) +
  geom_point(data = ceaf_df,
             aes(x = `WTP threshold`, y = CEAF),
             size = 0.1, stroke = 1,
             alpha = 1, shape = 1, color = "black", fill = 'black',
             show.legend = FALSE) +
  scale_shape_manual(values = c("CEAF" = "black")) +
  # geom_point(data = ceac_df,
  #            aes(x = `WTP threshold`, y = `Probability cost-effective`,
  #                shape = Option, color = Option),
  #            size = 1) +
  coord_cartesian(xlim = c(0, 10000), expand = FALSE) +
  scale_x_continuous(labels = dollar_format(prefix = "£")) +
  scale_y_continuous(labels = percent_format()) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8, 0.85),
    legend.text.align = 0, # 0 left (default), 1 right
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = "white", colour = "grey"),
    # legend.spacing = unit(0, "cm"), # spacing between legend items
    legend.spacing.y = unit(-0.2, "cm"), # bring legends closer
    legend.key.size = unit(0.2, "cm"),
    # legend.position = 'bottom',
    # legend.box.margin = margin(),
    # legend.direction = "horizontal",
    # panel.grid = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'black', fill = NA)) +
  labs(
    title = "Cost-effectiveness acceptability curve (CEAC)",
    x = "Willingness-to-pay",
    y = "Probability cost-effective") +
  guides(
    # Increase the size of the points in the legend:
    shape = guide_legend(override.aes = list(size = 2,
                                             #alpha = 1,
                                             stroke = 1,
                                             linetype = 0)),
    # line = guide_legend(override.aes = list(size = 0.2)),
    color = guide_legend(override.aes = list(size = 1,
                                             alpha = 1,
                                             shape = NA)))

ceac_plot

################

# CEAC with a CEAF:
ceac_plot = ggplot() +
  geom_line(data = ceac_df,
            aes(x = `WTP threshold`,
                y = `Probability cost-effective`,
                #group = Option,
                color = Option),
            size = 0.4) +
  # geom_point(data = ceac_df,
  #            aes(x = `WTP threshold`, y = `Probability cost-effective`,
  #                shape = Option, color = Option),
  #            size = 1) +
  coord_cartesian(xlim = c(0, 10000), expand = FALSE) +
  scale_x_continuous(labels = dollar_format(prefix = "£")) +
  scale_y_continuous(labels = percent_format()) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8, 0.85),
    legend.text.align = 0, # 0 left (default), 1 right
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = "white", colour = "grey"),
    # legend.spacing = unit(0, "cm"), # spacing between legend items
    legend.spacing.y = unit(-0.2, "cm"), # bring legends closer
    legend.key.size = unit(0.2, "cm"),
    # legend.position = 'bottom',
    # legend.box.margin = margin(),
    # legend.direction = "horizontal",
    # panel.grid = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.margin = unit(c(0,1,0,0), "cm")) + # more space LHS
  labs(
    title = "Cost-effectiveness acceptability curve (CEAC)",
    x = "Willingness-to-pay",
    y = "Probability cost-effective") +
  guides(
    # Increase the size of the points in the legend:
    color = guide_legend(override.aes = list(size = 1,
                                             alpha = 1,
                                             shape = NA)))
ceac_plot

ceac_ceaf_plot <- ceac_plot +
  geom_point(data = ceaf_df,
             aes(x = `WTP threshold`,
                 y = CEAF),
             size = 2, alpha = 0.8, shape = 21, color = "black",
             show.legend = TRUE) +
  geom_point(data = ceaf_df,
             aes(x = `WTP threshold`,
                 y = CEAF),
             size = 0.1, stroke = 1,
             alpha = 1, shape = 1, color = "black",
             show.legend = FALSE) +
  scale_fill_manual(values = c("CEAF" = "black")) +
  guides(
    # Increase the size of the points in the legend:
    shape = guide_legend(override.aes = list(size = 2,
                                             #alpha = 1,
                                             stroke = 1,
                                             linetype = 0)),
    fill = guide_legend(override.aes = list(#size = 0.3,
      #alpha = 1,
      stroke = 1,
      linetype = 0)))

ceac_ceaf_plot
##################################################################

tst = calculate_differentials_(.data = PSA_summary1$e, .ref = 1)

##################################################################
plot_CE_plane <- function(.PSA_dt, .ref = NULL, ...) {
  # Grab additional arguments:
  dots <- list(...)

  # Assign any additional arguments:
  .legend_pos <- if(is.null(dots[['.legend_pos']]) |
                    length(dots[['.legend_pos']]) != 2) {
    c(0.8, 0.2)
  } else {
    dots[['.legend_pos']]
  }
  .wtp <- if(is.null(dots[['.wtp_threshold']])) {
    c(20000, 30000, 50000)
  } else {
    dots[['.wtp_threshold']]
  }
  .seed_no <- if(is.null(dots[['.seed_no']])) {
    set.seed(1)
  } else {
    set.seed(dots[['.seed_no']])
  }
  .show_ICER <- if(is.null(dots[['.show_ICER']])) {
    TRUE
  } else {
    dots[['.show_ICER']]
  }

  # Prepare plot data:
  ## CE plot points:
  if(is.null(.ref)) {
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
  } else {
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
  }
  ## CE plot mean values:
  ce_plane_mean_dt <- ce_plane_dt %>%
    group_by(interventions) %>%
    summarise(
      Effects = mean(Effects),
      Costs = mean(Costs))
  ## CE plot willingness-to-pay values:
  .wtp = .wtp %>%
    as_tibble() %>%
    mutate(label = paste0("WTP threshold = £", .wtp),
           lty_ = "WTP threshold")
  ## CE plot x and y axis limits:
  x_lim = c(NA, max(ce_plane_dt$Effects) * 1.1)
  y_lim = c(NA, max(ce_plane_dt$Costs) * 1.1)

  # Plot:
  p <- ggplot() +
    geom_hline(yintercept = 0, colour = "dark gray") +
    geom_vline(xintercept = 0, colour = "dark gray") +
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
      data = ce_plane_mean_dt, inherit.aes = FALSE,
      aes(x = Effects,
          y = Costs,
          fill = interventions),
      shape = 21, colour = "black", show.legend = TRUE,
      size = 2, alpha = 1, stroke = 0.6) +
    ## Keep one value in the legend:
    scale_fill_discrete(
      breaks = ce_plane_mean_dt$interventions[1], # keep one
      labels = "Mean effects & costs") + # change its label
    #coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    geom_abline(
      data = .wtp,
      aes(intercept = 0,
          slope = value,
          linetype = lty_),
      show.legend = TRUE) +
    # scale_linetype_discrete(
    #   breaks = .wtp$label[1], # keep one
    #   labels = "WTP thresholds") + # change its label
    scale_linetype_manual(
      breaks = .wtp$lty_[1], # keep one
      #labels = "WTP thresholds", # change its label
      values = rep(3, nrow(.wtp))) +
    theme(
      legend.title = element_blank(),
      legend.position = .legend_pos,
      #legend.position = 'bottom',
      #legend.box.margin = margin(),
      #legend.box.spacing = unit(0.5, "cm"),
      legend.key = element_rect(fill = "white", colour = "grey"),
      #legend.justification = c("right", "top"),
      # Control legend text alignment:
      legend.text.align = 0, # 0 left (default), 1 right
      # Add a black box around the legend:
      #legend.background = element_rect(fill = NA, color = 'black'),
      legend.background = element_rect(fill = NA),
      legend.spacing = unit(0, "cm"), # spacing between legend items
      legend.spacing.y = unit(-0.195, "cm"), # bring legends closer
      legend.key.size = unit(0.35, "cm"),
      #legend.direction = "horizontal",
      #panel.grid = element_line(colour = 'grey'),
      panel.border = element_rect(colour = 'black', fill = NA)) +
    labs(title = "Cost-effectiveness plane",
         x = "Effects",
         y = "Costs") +
    guides(
      # Increase the size of the points in the legend:
      color = guide_legend(
        override.aes = list(size = 1.5,
                            alpha = 1,
                            stroke = NA, # remove storke
                            linetype = 0)), # remove line
      # Remove the fill colour in shape 21, generalising it to all options:
      fill = guide_legend(
        override.aes = list(size = 2.5,
                            alpha = 1,
                            fill = NA, # remove fill
                            #stroke = 1,
                            linetype = 0)), # remove line
      # Remove the storke from the line and adjust the size:
      linetype = guide_legend(
        override.aes = list(stroke = NA)))

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
        # nudge_x = 0.2,
        # nudge_y = 0.2,
        segment.curvature = 1e-8,
        arrow = arrow(length = unit(0.015, "npc")),
        max.overlaps = Inf,
        min.segment.length = 0)
  }

  return(p)
}

plot_CE_plane <- function(.PSA_dt, .ref = NULL, ...) {
  # Grab additional arguments:
  # expected_args <- c('.legend_pos', '.wtp_threshold', '.seed_no',
  #                    '.show_ICER', '.show_wtp', '.nudge_labels')
  default_args <- list('.legend_pos' = c(0.8, 0.2),
                       '.wtp_threshold' = c(20000, 30000, 50000),
                       '.seed_no' = 1,
                       '.show_ICER' = TRUE,
                       '.show_wtp' = TRUE,
                       '.nudge_labels' = c(0, 0))
  expected_args <- names(default_args)
  .args_ <- list(...)
  supplied_args <- names(.args_)
  if(any(!supplied_args %in% expected_args))
    message("Argument ",
            paste(supplied_args[!supplied_args %in% expected_args]),
            " is unknown, and therefore ignored")

  purrr::walk(
    .x = expected_args,
    .f = function(.arg) {
      assign(.arg,
             if(is.null(.args_[[.arg]])) {
               default_args[[.arg]]
             } else {
               .args_[[.arg]]
             }, env = sys.frame())
    })
  # dots <- list(...)
  # # Assign any additional arguments:
  # .legend_pos <- if(is.null(dots[['.legend_pos']]) |
  #                   length(dots[['.legend_pos']]) != 2) {
  #   c(0.8, 0.2)
  # } else {
  #   dots[['.legend_pos']]
  # }
  # .wtp <- if(is.null(dots[['.wtp_threshold']])) {
  #   c(20000, 30000, 50000)
  # } else {
  #   dots[['.wtp_threshold']]
  # }
  # .seed_no <- if(is.null(dots[['.seed_no']])) {
  #   set.seed(1)
  # } else {
  #   set.seed(dots[['.seed_no']])
  # }
  # .show_ICER <- if(is.null(dots[['.show_ICER']])) {
  #   TRUE
  # } else {
  #   dots[['.show_ICER']]
  # }
  # .show_wtp <- if(is.null(dots[['.show_wtp']])) {
  #   TRUE
  # } else {
  #   dots[['.show_wtp']]
  # }
  # .nudge_labels <- if(is.null(dots[['.nudge_labels']])) {
  #   NULL
  # } else {
  #   dots[['.nudge_labels']]
  # }
  #
  # Prepare plot data:
  ## CE plot points:
  if(is.null(.ref)) {
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
    .title_lab = "Cost Effectiveness Plane"
    .x_lab = "Effects"
    .y_lab = "Costs"
  } else {
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
  ## CE plot ICER labels nudgement:
  .nudge_labels[1] = max(ce_plane_dt$Effects) *
    .nudge_labels[1]
  .nudge_labels[2] = (max(ce_plane_dt$Costs) - min(ce_plane_dt$Costs)) *
    .nudge_labels[2]
  ## CE plot willingness-to-pay values:
  .wtp = .wtp_threshold %>%
    as_tibble() %>%
    mutate(x_cord = max(ce_plane_dt$Costs) /
             .wtp_threshold, # set location dynamically
           y_cord = max(ce_plane_dt$Costs), # set location dynamically
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
      data = ce_plane_mean_dt, inherit.aes = FALSE,
      aes(x = Effects,
          y = Costs,
          fill = interventions),
      shape = 21, colour = "black", show.legend = TRUE,
      size = 2, alpha = 1, stroke = 0.6) +
    ## Keep one value in the legend:
    scale_fill_discrete(
      breaks = ce_plane_mean_dt$interventions[1], # keep one
      labels = "Mean effects & costs") + # change its label
    coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    theme(
      legend.title = element_blank(),
      legend.position = .legend_pos,
      #legend.position = 'bottom',
      #legend.box.margin = margin(),
      #legend.box.spacing = unit(0.5, "cm"),
      legend.key = element_rect(fill = "white", colour = "grey"),
      #legend.justification = c("right", "top"),
      # Control legend text alignment:
      legend.text.align = 0, # 0 left (default), 1 right
      # Add a black box around the legend:
      #legend.background = element_rect(fill = NA, color = 'black'),
      legend.background = element_rect(fill = NA),
      legend.spacing = unit(0, "cm"), # spacing between legend items
      legend.spacing.y = unit(-0.195, "cm"), # bring legends closer
      legend.key.size = unit(0.35, "cm"),
      #legend.direction = "horizontal",
      #panel.grid = element_line(colour = 'grey'),
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
                            stroke = NA, # remove storke
                            linetype = 0)), # remove line
      # Remove the fill colour in shape 21, generalising it to all options:
      fill = guide_legend(
        override.aes = list(size = 2.5,
                            alpha = 1,
                            fill = NA, # remove fill
                            #stroke = 1,
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
      # scale_linetype_discrete(
      #   breaks = .wtp$label[1], # keep one
      #   labels = "WTP thresholds") + # change its label
      scale_linetype_manual(
        breaks = .wtp$lty_[1], # keep one
        #labels = "WTP thresholds", # change its label
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
        # Remove the stroke from the line and adjust the size:
        linetype = guide_legend(
          override.aes = list(stroke = NA)) # remove stroke
      )
  }

  return(p)
}

plot_CE_plane <- function(.PSA_dt, .ref = NULL, ...) {
  .env = environment()
  # Grab additional arguments:
  # expected_args <- c('.legend_pos', '.wtp_threshold', '.seed_no',
  #                    '.show_ICER', '.show_wtp', '.nudge_labels')
  default_args <- list('.legend_pos' = c(0.8, 0.2),
                       '.wtp_threshold' = c(20000, 30000),
                       '.seed_no' = 1,
                       '.show_ICER' = TRUE,
                       '.show_wtp' = TRUE,
                       '.nudge_labels' = NULL)
  expected_args <- names(default_args)
  .args_ <- list(...)
  supplied_args <- names(.args_)
  if(any(!supplied_args %in% expected_args))
    message("Argument ",
            paste(supplied_args[!supplied_args %in% expected_args]),
            " is unknown, and therefore ignored")

  purrr::walk(
    .x = expected_args,
    .f = function(.arg) {
      # cat("\n", .arg, " def:\n")
      # print(default_args[[.arg]])
      # cat("\n", .arg, " exp:\n")
      # print(.args_[[.arg]])
      assign(.arg,
             if(is.null(.args_[[.arg]])) {
               default_args[[.arg]]
             } else {
               .args_[[.arg]]
             }, envir = .env)
    })

  # dots <- list(...)
  # # Assign any additional arguments:
  # .legend_pos <- if(is.null(dots[['.legend_pos']]) |
  #                   length(dots[['.legend_pos']]) != 2) {
  #   c(0.8, 0.2)
  # } else {
  #   dots[['.legend_pos']]
  # }
  # .wtp <- if(is.null(dots[['.wtp_threshold']])) {
  #   c(20000, 30000, 50000)
  # } else {
  #   dots[['.wtp_threshold']]
  # }
  # .seed_no <- if(is.null(dots[['.seed_no']])) {
  #   set.seed(1)
  # } else {
  #   set.seed(dots[['.seed_no']])
  # }
  # .show_ICER <- if(is.null(dots[['.show_ICER']])) {
  #   TRUE
  # } else {
  #   dots[['.show_ICER']]
  # }
  # .show_wtp <- if(is.null(dots[['.show_wtp']])) {
  #   TRUE
  # } else {
  #   dots[['.show_wtp']]
  # }
  # .nudge_labels <- if(is.null(dots[['.nudge_labels']])) {
  #   NULL
  # } else {
  #   dots[['.nudge_labels']]
  # }
  #
  # Prepare plot data:
  ## CE plot points:
  if(is.null(.ref)) {
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
    .title_lab = "Cost Effectiveness Plane"
    .x_lab = "Effects"
    .y_lab = "Costs"
  } else {
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
  ## CE plot ICER labels nudgement:
  .nudge_labels[1] = max(ce_plane_dt$Effects) *
    .nudge_labels[1]
  .nudge_labels[2] = (max(ce_plane_dt$Costs) - min(ce_plane_dt$Costs)) *
    .nudge_labels[2]
  ## CE plot willingness-to-pay values:
  .wtp = .wtp_threshold %>%
    as_tibble() %>%
    mutate(x_cord = max(ce_plane_dt$Costs) /
             .wtp_threshold, # set location dynamically
           y_cord = max(ce_plane_dt$Costs), # set location dynamically
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
      data = ce_plane_mean_dt, inherit.aes = FALSE,
      aes(x = Effects,
          y = Costs,
          fill = interventions),
      shape = 21, colour = "black", show.legend = TRUE,
      size = 2, alpha = 1, stroke = 0.6) +
    ## Keep one value in the legend:
    scale_fill_discrete(
      breaks = ce_plane_mean_dt$interventions[1], # keep one
      labels = "Mean effects & costs") + # change its label
    coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    theme(
      legend.title = element_blank(),
      legend.position = .legend_pos,
      #legend.position = 'bottom',
      #legend.box.margin = margin(),
      #legend.box.spacing = unit(0.5, "cm"),
      legend.key = element_rect(fill = "white", colour = "grey"),
      #legend.justification = c("right", "top"),
      # Control legend text alignment:
      legend.text.align = 0, # 0 left (default), 1 right
      # Add a black box around the legend:
      #legend.background = element_rect(fill = NA, color = 'black'),
      legend.background = element_rect(fill = NA),
      legend.spacing = unit(0, "cm"), # spacing between legend items
      legend.spacing.y = unit(-0.195, "cm"), # bring legends closer
      legend.key.size = unit(0.35, "cm"),
      #legend.direction = "horizontal",
      #panel.grid = element_line(colour = 'grey'),
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
                            stroke = NA, # remove storke
                            linetype = 0)), # remove line
      # Remove the fill colour in shape 21, generalising it to all options:
      fill = guide_legend(
        override.aes = list(size = 2.5,
                            alpha = 1,
                            fill = NA, # remove fill
                            #stroke = 1,
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
      # scale_linetype_discrete(
      #   breaks = .wtp$label[1], # keep one
      #   labels = "WTP thresholds") + # change its label
      scale_linetype_manual(
        breaks = .wtp$lty_[1], # keep one for the legend
        #labels = "WTP thresholds", # change its label
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
        # Remove the stroke from the line and adjust the size:
        linetype = guide_legend(
          override.aes = list(stroke = NA)) # remove stroke
      )
  }

  return(p)
}

load_all()

PSA_summary = summarise_PSA_(
  .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
  .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
  .interventions = ShinyPSA::Vaccine_PSA$treats,
  .plot = TRUE)

PSA_summary = summarise_PSA_(
  .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
  .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
  .interventions = ShinyPSA::Smoking_PSA$treats,
  .plot = TRUE)

p = plot_CEplane(PSA_summary,
                  .ref = 2,
                  .show_ICER = T,
                  .legend_pos = c(0.8, 0.2),
                  .show_wtp = T,
                  .zoom = T,
                  .wtp_threshold = c(20000, 500, 100, 50),
                  #tst = "PRINT",
                  .nudge_labels = c(0.1, -0.1))
p
##################################################################

tmp <- function() {
  ################
  # CEAC:
  ceac_df = PSA_dt$CEAC %>%
    mutate('WTP threshold' = PSA_dt$k) %>%
    pivot_longer(cols = -`WTP threshold`,
                 names_to = 'Option',
                 values_to = 'Probability cost-effective')

  ceaf_df = PSA_dt$CEAF$ceaf %>%
    as_tibble() %>%
    mutate('WTP threshold' = PSA_dt$k) %>%
    rename('CEAF' = value)

  # CEAC with a CEAF:
  ceac_plot = ggplot() +
    geom_line(data = ceac_df,
              aes(x = `WTP threshold`,
                  y = `Probability cost-effective`,
                  #group = Option,
                  color = Option),
              size = 0.4) +
    # geom_point(data = ceac_df,
    #            aes(x = `WTP threshold`, y = `Probability cost-effective`,
    #                shape = Option, color = Option),
    #            size = 1) +
    coord_cartesian(xlim = c(0, 10000), expand = FALSE) +
    scale_x_continuous(labels = dollar_format(prefix = "£")) +
    scale_y_continuous(labels = percent_format()) +
    theme(
      legend.title = element_blank(),
      legend.position = c(0.8, 0.85),
      legend.text.align = 0, # 0 left (default), 1 right
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(fill = "white", colour = "grey"),
      # legend.spacing = unit(0, "cm"), # spacing between legend items
      legend.spacing.y = unit(-0.2, "cm"), # bring legends closer
      legend.key.size = unit(0.2, "cm"),
      # legend.position = 'bottom',
      # legend.box.margin = margin(),
      # legend.direction = "horizontal",
      # panel.grid = element_line(colour = 'grey'),
      panel.border = element_rect(colour = 'black', fill = NA),
      plot.margin = unit(c(0,1,0,0), "cm")) + # more space LHS
    labs(
      title = "Cost-effectiveness acceptability curve (CEAC)",
      x = "Willingness-to-pay",
      y = "Probability cost-effective") +
    guides(
      # Increase the size of the points in the legend:
      color = guide_legend(override.aes = list(size = 1,
                                               alpha = 1,
                                               shape = NA)))
  ceac_plot

  ceac_ceaf_plot <- ceac_plot +
    geom_point(data = ceaf_df,
               aes(x = `WTP threshold`,
                   y = CEAF),
               size = 2, alpha = 0.8, shape = 21, color = "black",
               show.legend = TRUE) +
    geom_point(data = ceaf_df,
               aes(x = `WTP threshold`,
                   y = CEAF),
               size = 0.1, stroke = 1,
               alpha = 1, shape = 1, color = "black",
               show.legend = FALSE) +
    scale_fill_manual(values = c("CEAF" = "black")) +
    guides(
      # Increase the size of the points in the legend:
      shape = guide_legend(override.aes = list(size = 2,
                                               #alpha = 1,
                                               stroke = 1,
                                               linetype = 0)),
      fill = guide_legend(override.aes = list(#size = 0.3,
        #alpha = 1,
        stroke = 1,
        linetype = 0)))

  ceac_ceaf_plot
}

load_all()

PSA_summary = summarise_PSA_(
  .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
  .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
  .interventions = ShinyPSA::Vaccine_PSA$treats)

PSA_summary = summarise_PSA_(
  .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
  .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
  .interventions = ShinyPSA::Smoking_PSA$treats)














