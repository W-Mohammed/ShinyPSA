################################################################################
#
# Script Name:        test-get_icers.R
# Module Name:        Economic/PSA
# Script Description: Tests the set of functions defined in get_icers.R.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

test_that("Identifying dominance", {
  # Define a dataset to test the function:
  icer_test_input <-
    tibble("intervention" = paste("intervention", 1:8),
           "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
           "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12))

  icer_test_input <- icer_test_input %>%
    mutate(delta.e = NA_real_,
           delta.c = NA_real_,
           dominance = NA_character_,
           icer = NA_real_,
           icer_label = NA_character_)

  # Define expected outputs:
  icer_output_tab <- icer_test_input %>%
    arrange(qalys)
  icer_output_tab$dominance <-
    c(NA, "dominated", NA, "dominated", NA, "dominated", NA, NA)
  icer_output_tab$icer_label <-
    c(NA, "dominated by intervention 3", NA, "dominated by intervention 6",
      NA, "dominated by intervention 7", NA, NA)

  # Test:
  expect_equal(icer_test_input %>%
                 identify_dominance(),
               icer_output_tab)
})

test_that("Identifying extended dominance", {
  # Define a dataset to test the function:
  icer_test_input <-
    tibble("intervention" = paste("intervention", 1:8),
           "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
           "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12))

  icer_test_input <- icer_test_input %>%
    arrange(qalys) %>%
    mutate(delta.e = NA_real_,
           delta.c = NA_real_,
           dominance = NA_character_,
           icer = NA_real_,
           icer_label = NA_character_)

  icer_test_input$dominance <-
    c(NA, "dominated", NA, "dominated", NA, "dominated", NA, NA)
  icer_test_input$icer <-
    c(NA, NA, 7804.8780, NA, -227.2727, NA, 3181.8182, 232.5581)
  icer_test_input$icer_label <-
    c(NA, "dominated by intervention 3", NA, "dominated by intervention 6",
      NA, "dominated by intervention 7", NA, NA)

  # Define expected outputs:
  icer_output_tab <- icer_test_input %>%
    arrange(qalys)
  icer_output_tab$dominance <-
    c(NA, "dominated", "e.dominated", "dominated", NA, "dominated",
      "e.dominated", NA)
  icer_output_tab$icer_label <-
    c(NA, "dominated by intervention 3", "e.dominated by intervention 6",
      "dominated by intervention 6", NA, "dominated by intervention 7",
      "e.dominated by intervention 8", NA)

  #Test:
  expect_equal(icer_test_input %>%
                 identify_e.dominance(),
               icer_output_tab)
})

test_that("Computing icer(s)", {
  # Define a dataset to test the function:
  icer_test_input <-
    tibble("intervention" = paste("intervention", 1:8),
           "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
           "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12))

  icer_test_input <- icer_test_input %>%
    arrange(qalys) %>%
    mutate(delta.e = NA_real_,
           delta.c = NA_real_,
           dominance = NA_character_,
           icer = NA_real_,
           icer_label = NA_character_)

  icer_test_input$dominance <-
    c(NA, "dominated", NA, "dominated", NA, "dominated", NA, NA)
  icer_test_input$icer_label <-
    c(NA, "dominated by intervention 3", NA, "dominated by intervention 6",
      NA, "dominated by intervention 7", NA, NA)

  # Define expected outputs:
  icer_output_tab <- icer_test_input
  icer_output_tab$delta.e <-
    c(NA, NA, 0.41, NA, 0.88, NA, 2.20, 4.30)
  icer_output_tab$delta.c <-
    c(NA, NA, 3200, NA, -200, NA, 7000, 1000)
  icer_output_tab$icer <-
    c(NA, NA, 7804.8780, NA, -227.2727, NA, 3181.8182, 232.5581)
  icer_output_tab$icer_label <-
    c(NA, "dominated by intervention 3", "ICER vs intervention 1",
      "dominated by intervention 6", "ICER vs intervention 3",
      "dominated by intervention 7", "ICER vs intervention 6",
      "ICER vs intervention 7")

  # Test:
  expect_equal(icer_test_input %>%
                 compute_ICERs(),
               icer_output_tab)
})

test_that("Getting icer(s)", {
  # Define a dataset to test the function:
  icer_test_input <-
    tibble("intervention" = paste("intervention", 1:8),
           "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
           "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12))

  icer_test_input <- icer_test_input %>%
    mutate(delta.e = NA_real_,
           delta.c = NA_real_,
           dominance = NA_character_,
           icer = NA_real_,
           icer_label = NA_character_)

  # Define expected outputs:
  icer_output_tab <- icer_test_input %>%
    arrange(qalys)

  icer_output_tab$delta.e <-
    c(NA, NA, NA, NA, NA, NA, NA, 7.79)
  icer_output_tab$delta.c <-
    c(NA, NA, NA, NA, NA, NA, NA, 11000)
  icer_output_tab$dominance <-
    c(NA, "dominated", "dominated", "dominated", "e.dominated",
      "dominated", "e.dominated", NA)
  icer_output_tab$icer <-
    c(NA, NA, NA, NA, NA, NA, NA, 1412.06675224646)
  icer_output_tab$icer_label <-
    c(NA, "dominated by intervention 3", "dominated by intervention 6",
      "dominated by intervention 6", "e.dominated by intervention 8",
      "dominated by intervention 7", "e.dominated by intervention 8",
      "ICER vs intervention 1")

  expect_equal(icer_test_input %>%
                 get_icers(),
               icer_output_tab)
})
