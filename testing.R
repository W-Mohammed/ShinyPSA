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
                           interventions = ShinyPSA::Vaccine_PSA$treats)
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
