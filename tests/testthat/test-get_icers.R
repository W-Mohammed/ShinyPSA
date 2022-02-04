################################################################################
#
# Script Name:        test-get_icers.R
# Module Name:        Economic/PSA
# Script Description: Tests the set of functions defined in get_icers.R.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

test_that("Identify dominance", {
  # Define a dataset to test the function:
  icer_test_input <-
    tibble(
      "intervention" = paste0("intervention", 1:8),
      "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
      "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12))

  # Define expected outputs:
  icer_output_tab <-
    c(NA, "dominated", NA, "dominated", NA, "dominated", NA, NA)

  # Test:
  expect_equal(identify_dominance(icer_data = icer_test_input %>%
                                    arrange(qalys) %>%
                                    mutate(delta.e = NA,
                                           delta.c = NA,
                                           dominated = NA,
                                           e.dominated = NA,
                                           icer = NA)),
               icer_output_tab)
})

test_that("Identify extended dominance", {
  # Define a dataset to test the function:
  icer_test_input <-
    tibble(
      "intervention" = paste0("intervention", 1:8),
      "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
      "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12))
  icer_test_input <- icer_test_input %>%
    arrange(qalys) %>%
    mutate(delta.e = NA,
           delta.c = NA,
           dominated = NA,
           e.dominated = NA,
           icer = NA)
  # Compute ICER(s), before extended dominance checking:
  icer_test_input[is.na(icer_test_input$dominated) &
                    is.na(icer_test_input$e.dominated),
           c("delta.e", "delta.c", "icer")] <- icer_test_input %>%
    compute_ICERs()

  # Define expected outputs:
  icer_output_tab <-
    c(NA, "e.dominated", NA, "e.dominated", NA, "e.dominated", NA, NA)

  expect_equal(identify_e.dominance(icer_data = icer_test_input),
               icer_output_tab)
})

test_that("Computing icer(s)", {
  # Define a dataset to test the function:
  icer_test_input <-
    tibble(
      "intervention" = paste0("intervention", 1:8),
      "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
      "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12),
  "delta.e" = c(NA, NA, NA, NA, NA, NA, NA, NA),
  "delta.c" = c(NA, NA, NA, NA, NA, NA, NA, NA),
  "dominated" = c(NA, NA, NA, NA, NA, NA, NA, NA),
  "e.dominated" = c(NA, NA, NA, NA, NA, NA, NA, NA),
  "icer" = c(NA, NA, NA, NA, NA, NA, NA, NA))

  # Define expected outputs:
  icer_output_tab <-
    matrix(data = c(NA, 0.223, 0.187, 2.205, -1.999, 0.674, 2.200, 4.300, NA, 5777, -2577, 9371, 135, -9706, 7000, 1000, NA, 25905.82960, -13780.74866, 4249.88662, -67.53377, -14400.59347, 3181.81818, 232.55814), ncol = 3)
  dimnames(icer_output_tab) <- list(NULL, c("delta.e", "delta.c", "icer"))

  expect_equal(compute_ICERs(icer_data = icer_test_input), icer_output_tab)
})

test_that("Getting icer(s)", {
  # Define a dataset to test the function:
  icer_test_input <-
    tibble(
      "intervention" = paste0("intervention", 1:8),
      "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
      "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12))

  # Define expected outputs:
  icer_output_tab <-
    tibble(
    "intervention" = c("intervention1", "intervention2", "intervention3", "intervention5", "intervention6", "intervention4", "intervention7", "intervention8"),
    "costs" = c(0, 5777, 3200, 12706, 3000, 12571, 10000, 11000),
    "qalys" = c(4.21, 4.433, 4.62, 4.826, 5.5, 6.825, 7.7, 12),
    "delta.e" = c(NA, NA, NA, NA, NA, NA, NA, 7.79),
    "delta.c" = c(NA, NA, NA, NA, NA, NA, NA, 11000),
    "dominated" = c(NA, "dominated", "dominated", "dominated", NA, "dominated", NA, NA),
    "e.dominated" = c(NA, NA, NA, NA, "e.dominated", NA, "e.dominated", NA),
    "icer" = c(NA, NA, NA, NA, NA, NA, NA, 1412.06675224646)
    )
  expect_equal(get_icers(icer_data = icer_test_input), icer_output_tab)
})
