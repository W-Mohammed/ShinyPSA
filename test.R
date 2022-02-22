pacman::p_load(BCEA, tidyverse, devtools, ggrepel)

icer_tmp <- tibble(
  'intervention' = paste("int", 1:4),
  'qalys' = NA,
  'costs' = NA) %>%
  add_missing_columns_(.x = .,
                       .characters = c("dominance", "icer_label"),
                       .numerics = c("delta.e", "delta.c", "icer"))




icer_test_input <-
  tibble("intervention" = paste("intervention", 1:2),
         "costs" = c(500, 5777),
         "qalys" = c(4.21, 4.133)) %>%
  mutate(delta.e = NA_real_,
         delta.c = NA_real_,
         dominance = NA_character_,
         icer = NA_real_,
         icer_label = NA_character_)
View(icer_test_input %>% dominance_wraper())

icer_test_input <-
  tibble("intervention" = paste("intervention", 1:5),
         "costs" = c(500, 600, 700, 800, 900),
         "qalys" = c(4.21, 4.133, 4.1, 4.05, 4))
View(icer_test_input %>% get_icers())
icer_test_input <-
  tibble("intervention" = paste("intervention", 1:6),
         "costs" = c(500, 600, 700, 800, 900, 1000),
         "qalys" = c(4.21, 4.133, 4.1, 4.05, 4, 5))
View(icer_test_input %>% get_icers())
# icer_test_input %>% dominance_wraper()
character_nms <- c("dominance", "icer_label")
numeric_nms <- c("delta.e", "delta.c", "icer")
icer_test_input %>% naming_helper(.characters = character_nms, .numerics = numeric_nms)
nms <- c(character_nms, numeric_nms)
missing_nms <- setdiff(nms, names(icer_test_input))
icer_test_input[missing_nms[missing_nms %in% numeric_nms]] <- NA_real_
icer_test_input[missing_nms[missing_nms %in% character_nms]] <- NA_character_

# fs = function(.data, .vars) {
#   for (.i in .vars) {
#     .data = .data %>%
#       mutate({{.i}} := NA)
#   }
#   return(.data)
# }
# fs(icer_test_input, syms(setdiff(nms, names(icer_test_input))))
# syms(setdiff(nms, names(icer_test_input)))


fs = function(.data, .vars) {
  .data = .data %>%
    mutate(across({{.vars}}, .fns = NA))
  return(.data)
}

fs(icer_test_input, setdiff(nms, names(icer_test_input)))
syms(setdiff(nms, names(icer_test_input)))
unlist(map(setdiff(nms, names(icer_test_input)), sym))


View(icer_test_input %>% compute_ICERs(.incremental = F))

icer_test_input <-
  tibble("intervention" = paste("intervention", 1:8),
         "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
         "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12))

icer_test_input %>% identify_dominance_() %>% View()
.x = icer_test_input
# Check if missing key columns and create them if so:
.x <- .x %>%
  add_missing_columns_(.x = .,
                       .characters = c("dominance", "icer_label"),
                       .numerics = c("delta.e", "delta.c", "icer"))

# Check for unidentified dominance
while (any("dominated" %in%
           (.x %>%
            filter(if_any(dominance, ~ is.na(.))) %>%
            identify_dominance_() %>%
            pull(dominance)))) {
  # Do until all dominated are identified
  .x <- .x %>%
    identify_dominance_()
}
.x %>%
  filter(if_any(dominance, ~ is.na(.))) %>%
  identify_dominance_()
identify_dominance_(.icer_data = icer_test_input)



icer_df <- tibble(
  'intervention' = interventions,
  'qalys' = colMeans(e),
  'costs' = colMeans(c)
)
ICER <- get_icers(icer_data = icer_df)

pacman::p_load(BCEA, tidyverse, devtools, ggrepel)
# Tidyverse version of nmb, ceac, ceaf and evpi:
data(Smoking)
v.k = seq(from = 0, to = 500, length.out = 11)
names(v.k) = paste0("£", v.k)
effs = as_tibble(e); costs = as_tibble(c)
colnames(effs) <- colnames(costs) <- treats
# Get NMBs:
test = map2(.x = effs, .y = costs, .f = function(.effs = .x, .costs = .y){
  map_dfc(as.list(v.k), .f = function(.k = .x) {
    .effs * .k - .costs
  })
}) # outer list by intervention
# What if we wanted to transpose:
test1 = test %>% transpose() # outer list by k (threshold)
# Get expected NMBs:
test2 = map_dfr(test1, .f = function(.x) {
  colMeans(as_tibble(.x))
})

# Possible use in a CEAC:
test3 = test1 %>%
  map_dfr(.f = function(.x) {
    .tmp = max.col(as_tibble(.x), ties.method = "first")
    table(.tmp) / length(.tmp)}) %>%
  select(paste(1:length(test1[[1]]))) %>% # arrange for column names
  `colnames<-`(treats) %>%
  mutate(across(.fns = ~ ifelse(is.na(.x), 0, .x)))


# Possible use in a CEAF:
test4 = do.call(pmax, test3)
# Find best:
test5 = max.col(test3)
best = treats[test5]
# Find k values where decision changed:
changed = c(0, diff(test5))
kStar = v.k[changed != 0]
# Calculate EVPI:
test6_max = test1 %>%
  map_dfr(.f = function(.x) {
    do.call(pmax, as_tibble(.x))
  })
# test6_best = test1 %>%
#   map2_dfc(.x = ., .y = test5, .f = function(.x, .y){
#     unname(.x[.y])
#   })

test6_ol = pmap_dfc(.l = list(test1, test5, test6_max),
                    .f = function(first, second, third){
                      third - first[[second]]
                    })

test6_ol2 = pmap_dfc(.l = list(test1, test5, test6_max),
                     .f = function(x, y, z){
                       z - x[[y]]
                     })

test6_ol3 = pmap_dfc(.l = list(test1, test5, test6_max),
                     .f = function(.x, .y, .z){
                       .z - .x[[.y]]
                     })

test6_vi = map2_dfc(.x = test6_max, .y = test1, .f = function(.x, .y){
  .x - max(colMeans(as_tibble(.y)))
})
# Check ol and vi give equal values:
identical(round(colMeans(test6_vi), digits = 10), round(colMeans(test6_ol3), digits = 10))
compare(test6_vi, test6_ol3, ignoreNames = T, coerce = T)
# Expected vi:
test6_evi = colMeans(test6_vi)

# costs and qalys deferentials:
.ref = 1
eee_s = as_tibble(e) %>%
  `colnames<-`(treats)
delta.effs <- eee_s %>%
  select(-.ref) %>%
  mutate(across(.fns =
                  ~ eee_s %>% select(all_of(.ref)) - .x))

# compare(delta.effs, as_tibble(tst_bcea$delta_e), ignoreNames = T, coerce = T)

ccc_s = as_tibble(c) %>%
  `colnames<-`(treats)
delta.costs <- ccc_s %>%
  select(-.ref) %>%
  mutate(across(.fns =
                  ~ ccc_s %>% select(all_of(.ref)) - .x))

tst_PSA = summarise_PSA(.effs = e, .costs = c, .interventions = treats)
tst_pSA2 = summarise_PSA(.effs = e, .costs = c, .interventions = treats,
                         .incremental = FALSE, .ref = 1)
tst_bcea = bcea(eff = e, cost = c, ref = 1)
tst_bcea_p = bcea(eff = e, cost = c, ref = 1, plot = T)
ceplane.plot(tst_bcea)
ceef.plot(tst_bcea)
ceaf.plot(tst_bcea)

# Compare Base and Tidy versions of the PSA functions:
base_tidy_summarise = microbenchmark::microbenchmark(
  'base' = summarise_PSA(.effs = e, .costs = c, .interventions = treats),
  'tidy' = summarise_PSA_(.effs = e, .costs = c, .interventions = treats),
  times = 100)

plot(base_tidy_summarise)

base_tidy_ICERs = microbenchmark::microbenchmark(
  'base' = compute_ICERs(.icer_data = NULL, .effs = e, .costs = c, .interventions = treats),
  'tidy' = compute_ICERs_(.icer_data = NULL, .effs = as_tibble(e), .costs = as_tibble(c), .interventions = treats),
  times = 100)

plot(base_tidy_ICERs)

base_tidy_NMBs = microbenchmark::microbenchmark(
  'base' = compute_NMBs(.effs = e, .costs = c, .interventions = treats),
  'tidy' = compute_NMBs_(.effs = as_tibble(e), .costs = as_tibble(c), .interventions = treats),
  times = 100)

plot(base_tidy_NMBs)

base_tidy_CEACs = microbenchmark::microbenchmark(
  'base' = compute_CEACs(.nmb = NULL, .effs = e, .costs = c, .interventions = treats),
  'tidy' = compute_CEACs_(.nmb = NULL, .effs = as_tibble(e), .costs = as_tibble(c), .interventions = treats),
  times = 100)

plot(base_tidy_CEACs)

base_tidy_EVPIs = microbenchmark::microbenchmark(
  'base' = compute_EVPIs(.effs = e, .costs = c, .interventions = treats),
  'tidy' = compute_EVPIs_(.effs = as_tibble(e), .costs = as_tibble(c), .interventions = treats),
  times = 100)

plot(base_tidy_EVPIs)

# Plots:
ce_plane_dt = tst_tidy2$e %>%
  `colnames<-`(paste0(1:ncol(.),": ", colnames(.))) %>%
  mutate(sims = row_number()) %>%
  pivot_longer(cols = -sims, names_to = "interventions",
               values_to = "effects") %>%
  left_join(x = .,
            y = tst_tidy2$c %>%
              `colnames<-`(paste0(1:ncol(.),": ", colnames(.))) %>%
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

# ce_plane$interventions[ce_plane$interventions == "V1"] = treats[1]
# ce_plane$interventions[ce_plane$interventions == "V2"] = treats[2]
# ce_plane$interventions[ce_plane$interventions == "V3"] = treats[3]
# ce_plane$interventions[ce_plane$interventions == "V4"] = treats[4]
set.seed(4)
ce_plane_plot <- ggplot() +
  geom_hline(yintercept = 0, colour = "dark gray") +
  geom_vline(xintercept = 0, colour = "dark gray") +
  geom_point(data = ce_plane_dt,
             aes(x = effects, y = costs, color = interventions),
             size = 1, alpha = 0.5) +
  geom_point(data = tst_tidy2$ICER, inherit.aes = FALSE,
             aes(x = qalys, y = costs, fill = intervention),
             shape = 21, colour = "black", show.legend = TRUE,
             size = 1, alpha = 1, stroke = 1) +
  # Keep one value in the legend:
  scale_fill_discrete(
    breaks = tst_tidy2$ICER$intervention[1], # keep one
    labels = "Mean effects & costs") + # change its label
  #coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
  geom_text_repel(data = tst_tidy2$ICER,
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
    legend.box.margin = margin(),
    #legend.box.spacing = unit(0.5, "cm"),
    #legend.key = element_rect(fill = "white", colour = "black"),
    #legend.justification = c("right", "top"),
    # Control legend text alignment:
    legend.text.align = 0, # 0 left (default), 1 right
    # Add a black box around the legend:
    #legend.background = element_rect(fill = NA, color = 'black'),
    legend.background = element_rect(fill = NA),
    legend.spacing = unit(0, "cm"), # spacing between legend items
    legend.spacing.y = unit(-0.2, "cm"), # bring legends closer
    legend.key.size = unit(0, "cm"),
    #legend.direction = "horizontal",
    panel.grid = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'black', fill = NA)) +
  labs(title = "Cost-effectiveness plane",
       x = "Effects",
       y = "Costs") +
  guides(
    # Increase the size of the points in the legend:
    color = guide_legend(override.aes = list(size = 2,
                                             alpha = 1,
                                             stroke = NA, # remove storke
                                             linetype = 0)), # remove line
    # Remove the fill colour in shape 21, generalising it to all options:
    fill = guide_legend(override.aes = list(size = 2,
                                            alpha = 1,
                                            fill = NA, # remove fill
                                            stroke = 1, # remove line
                                            linetype = 0)),
    # Remove the storke from the line and adjust the size:
    linetype = guide_legend(override.aes = list(size = 0.7,
                                                stroke = NA)))

ceac_df = tst_tidy2$CEAC %>%
  mutate('WTP threshold' = tst_tidy2$k) %>%
  pivot_longer(cols = -`WTP threshold`,
               names_to = 'Option',
               values_to = 'Probability cost-effective')

ceaf_df = tst_tidy2$CEAF$ceaf %>%
  as_tibble() %>%
  mutate('WTP threshold' = tst_tidy2$k) %>%
  rename('CEAF' = value)

ceac_plot = ggplot() +
  geom_line(data = ceaf_df,
            aes(x = `WTP threshold`, y = CEAF),
            size = 2, alpha = 0.6, color = 'sky blue',
            show.legend = TRUE) +
  scale_fill_manual(values = c("CEAF" = "gray")) +
  geom_line(data = ceac_df,
            aes(x = `WTP threshold`, y = `Probability cost-effective`,
                group = Option, linetype = Option),
            size = 0.4) +
  geom_point(data = ceac_df,
             aes(x = `WTP threshold`, y = `Probability cost-effective`,
                 shape = Option, color = Option),
             size = 1) +
  coord_cartesian(xlim = c(0, 10000), expand = FALSE) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8, 0.85),
    legend.text.align = 0, # 0 left (default), 1 right
    legend.background = element_rect(fill = NA),
    # legend.spacing = unit(0, "cm"), # spacing between legend items
    legend.spacing.y = unit(-0.2, "cm"), # bring legends closer
    legend.key.size = unit(0.2, "cm"),
    # legend.position = 'bottom',
    # legend.box.margin = margin(),
    # legend.direction = "horizontal",
    panel.grid = element_line(colour = 'grey'),
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

bcea(eff = e, cost = c,interventions = treats, plot = T)
