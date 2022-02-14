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

icer_df <- tibble(
  'intervention' = interventions,
  'qalys' = colMeans(e),
  'costs' = colMeans(c)
)
ICER <- get_icers(icer_data = icer_df)
