library(ShinyPSA)

temp = ShinyPSA::summarise_PSA_(
  .effs = ShinyPSA::Brennan_10K_PSA$e,
  .costs = ShinyPSA::Brennan_10K_PSA$c,
  .params = NULL,
  .interventions = ShinyPSA::Brennan_10K_PSA,
  .plot = FALSE)

tem_tab = ShinyPSA::draw_summary_table_(
  .PSA_data = temp,
  .latex_subtitle_ = "This is a subtitle",
  .latex_ = TRUE,
  .latex_code_ = FALSE,
  .dominance_footnote_ = FALSE,
  .footnotes_sourcenotes_ = TRUE,
  .all_sourcenotes_ = T,
  .subset_tab_ = TRUE,
  .subset_group_ = c("NetBenefit",
                     "ProbabilityCE"))
#.latex_title_ = "This is a title",
#.latex_subtitle_ = "This is a subtitle"
#) #%>%
# gt::opt_align_table_header(align = "left") %>%
# gt::tab_style(
#   style = gt::cell_text(weight = "bold"),
#   locations = list(
#     gt::cells_column_labels(),
#     gt::cells_row_groups()))

tem_tab

gt::gtcars %>%
  dplyr::select(model, year, hp, trq) %>%
  dplyr::slice(1:8) %>%
  dplyr::mutate(
    row1 = "Numbered",
    row2 = dplyr::case_when(
      dplyr::row_number() < 5 ~ "top",
      TRUE ~ "false")) %>%
  dplyr::group_by(row1, row2) %>%
  gt::gt(rowname_col = "model")


# gt::tab_row_group(
#   label = "numbered",
#   rows = gt::matches("^[0-9]")
# )

## CR plots:
psa_sum_object <- ShinyPSA::summarise_PSA_(
  .effs = CRS_true_PSA$e,
  .costs = CRS_true_PSA$c,
  .params = CRS_true_PSA$p,
  .interventions = CRS_true_PSA$treats)
# CEP:
CE_plane <- ShinyPSA::plot_CEplane_(
  psa_sum_object,
  .legend_pos = c(0.3, 0.8),
  .show_ICER = FALSE,
  .show_title = FALSE)
# CEAC
CEAC <- ShinyPSA::plot_CEAC_(
  psa_sum_object,
  .legend_pos = "none",
  .show_title = FALSE)

ggplot2::ggsave(
  filename = "true_ce_plane.jpeg",
  plot = CE_plane,
  scale = 1.5,
  width = 1000,
  height = 700,
  units = "px")

ggplot2::ggsave(
  filename = "true_ceac.jpeg",
  plot = CEAC,
  scale = 1.5,
  width = 1000,
  height = 700,
  units = "px")
