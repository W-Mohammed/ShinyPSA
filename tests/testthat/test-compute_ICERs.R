################################################################################
#
# Script Name:        test-compute_ICERs.R
# Module Name:        Economic/PSA
# Script Description: Tests functions defined in compute_ICERs.R
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
  icer_output_tab1 <- icer_test_input %>%
    arrange(qalys)
  icer_output_tab1$dominance <-
    c(NA, "dominated", NA, "dominated", NA, "dominated", NA, NA)
  icer_output_tab1$icer_label <-
    c(NA, "dominated by intervention 3", NA, "dominated by intervention 6",
      NA, "dominated by intervention 7", NA, NA)
  icer_output_tab2 <- icer_output_tab1
  icer_output_tab2$dominance <-
    c(NA, "dominated", "dominated", "dominated", NA, "dominated", NA, NA)
  icer_output_tab2$icer_label <-
    c(NA, "dominated by intervention 3", "dominated by intervention 6",
      "dominated by intervention 6", NA, "dominated by intervention 7",
      NA, NA)

  # Tests:
  # identify_dominance:
  expect_equal(icer_test_input %>%
                 identify_dominance(),
               icer_output_tab1)
  # dominance_wraper:
  expect_equal(icer_test_input %>%
                 dominance_wraper(),
               icer_output_tab2)
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
  icer_output_tab1 <- icer_test_input %>%
    arrange(qalys)
  icer_output_tab1$dominance <-
    c(NA, "dominated", "e.dominated", "dominated", NA, "dominated",
      "e.dominated", NA)
  icer_output_tab1$icer_label <-
    c(NA, "dominated by intervention 3", "e.dominated by intervention 6",
      "dominated by intervention 6", NA, "dominated by intervention 7",
      "e.dominated by intervention 8", NA)
  icer_output_tab2 <- icer_output_tab1
  icer_output_tab2$dominance <-
    c(NA, "dominated", "e.dominated", "dominated", "e.dominated",
      "dominated","e.dominated", NA)
  icer_output_tab2$delta.e <- c(rep(NA, 7), 7.79)
  icer_output_tab2$delta.c <- c(rep(NA, 7), 11000)
  icer_output_tab2$icer <- c(rep(NA, 7), 1412.06675)
  icer_output_tab2$icer_label <-
    c("ICER reference", "dominated by intervention 3",
      "e.dominated by intervention 6", "dominated by intervention 6",
      "e.dominated by intervention 8", "dominated by intervention 7",
      "e.dominated by intervention 8", "ICER vs intervention 1")

  # Tests:
  # identify_e.dominance:
  expect_equal(icer_test_input %>%
                 identify_e.dominance(),
               icer_output_tab1)
  # e.dominance_wraper:
  expect_equal(icer_test_input %>%
                 e.dominance_wraper(),
               icer_output_tab2)
})

test_that("Calculating icer(s)", {
  # Define a dataset to test the function:
  icer_test_input <-
    tibble("intervention" = paste("intervention", 1:8),
           "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
           "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12)) %>%
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
  icer_output_tab1 <- icer_test_input
  icer_output_tab1$delta.e <-
    c(NA, NA, 0.41, NA, 0.88, NA, 2.20, 4.30)
  icer_output_tab1$delta.c <-
    c(NA, NA, 3200, NA, -200, NA, 7000, 1000)
  icer_output_tab1$icer <-
    c(NA, NA, 7804.8780, NA, -227.2727, NA, 3181.8182, 232.5581)
  icer_output_tab1$icer_label <-
    c("ICER reference", "dominated by intervention 3",
      "ICER vs intervention 1", "dominated by intervention 6",
      "ICER vs intervention 3", "dominated by intervention 7",
      "ICER vs intervention 6", "ICER vs intervention 7")

  icer_output_tab2 <- icer_test_input
  icer_output_tab2$delta.e <-
    c(0.000, 0.223, 0.410, 0.616, 1.290, 2.615, 3.490, 7.790)
  icer_output_tab2$delta.c <-
    c(0, 5777, 3200, 12706, 3000, 12571, 10000, 11000)
  icer_output_tab2$dominance <-
    c(rep("Not checked", nrow(icer_output_tab2)))
  icer_output_tab2$icer <-
    c(NA, 25905.82960, 7804.87805, 20626.62338, 2325.58140, 4807.26577,
      2865.32951, 1412.06675)
  icer_output_tab2$icer_label <-
    c("ICER(s) reference",
      rep("ICER reference is intervention 1", nrow(icer_output_tab2) - 1))

  icer_output_tab3 <- icer_test_input
  icer_output_tab3$delta.e <-
    c(-1.290, -1.067, -0.880, -0.674, 0.000, 1.325, 2.200, 6.500)
  icer_output_tab3$delta.c <-
    c(-3000, 2777, 200, 9706, 0, 9571, 7000, 8000)
  icer_output_tab3$dominance <-
    c(rep("Not checked", nrow(icer_output_tab3)))
  icer_output_tab3$icer <-
    c(2325.5814, -2602.6242, -227.2727, -14400.5935, NA, 7223.3962,
      3181.8182, 1230.7692)
  icer_output_tab3$icer_label <-
    c(rep("ICER reference is intervention 6", 4),
      "ICER(s) reference", rep("ICER reference is intervention 6", 3))

  # Tests:
  # Incremental analysis:
  expect_equal(icer_test_input %>%
                 calculate_ICERs(),
               icer_output_tab1)
  # No incremental analysis:
  # not choosing a reference intervention:
  expect_equal(icer_test_input %>%
                 calculate_ICERs(.incremental = FALSE),
               icer_output_tab2)
  # choosing a reference intervention:
  expect_equal(icer_test_input %>%
                 calculate_ICERs(.incremental = FALSE, .ref = 5),
               icer_output_tab3)
})

test_that("Computing icer(s)", {
  # Define a dataset to test the function:
  icer_test_input1 <-
    tibble("intervention" = paste("intervention", 1:8),
           "costs" = c(0, 5777, 3200, 12571, 12706, 3000, 10000, 11000),
           "qalys" = c(4.21, 4.433, 4.62, 6.825, 4.826, 5.5, 7.7, 12)) %>%
    arrange(qalys) %>%
    mutate(delta.e = NA_real_,
           delta.c = NA_real_,
           dominance = NA_character_,
           icer = NA_real_,
           icer_label = NA_character_)

  icer_test_input2 <-
    tibble("intervention" = paste("intervention", 1:2),
           "costs" = c(500, 5777),
           "qalys" = c(4.21, 4.133))

  # Define expected outputs:
  icer_output_tab1 <- icer_test_input1

  icer_output_tab1$delta.e <-
    c(NA, NA, NA, NA, NA, NA, NA, 7.79)
  icer_output_tab1$delta.c <-
    c(NA, NA, NA, NA, NA, NA, NA, 11000)
  icer_output_tab1$dominance <-
    c(NA, "dominated", "dominated", "dominated", "e.dominated",
      "dominated", "e.dominated", NA)
  icer_output_tab1$icer <-
    c(NA, NA, NA, NA, NA, NA, NA, 1412.06675224646)
  icer_output_tab1$icer_label <-
    c("ICER reference", "dominated by intervention 3",
      "dominated by intervention 6", "dominated by intervention 6",
      "e.dominated by intervention 8", "dominated by intervention 7",
      "e.dominated by intervention 8", "ICER vs intervention 1")

  icer_output_tab2 <- icer_test_input2 %>%
    arrange(qalys) %>%
    mutate(delta.e = NA_real_,
           delta.c = NA_real_,
           icer = NA_real_,
           dominance = NA_character_,
           icer_label = NA_character_) %>%
    mutate(dominance = c("dominated", NA),
           icer_label = c("dominated by intervention 1", NA))

  icer_output_tab3 <- icer_test_input1
  icer_output_tab3$delta.e <-
    c(0.000, 0.223, 0.410, 0.616, 1.290, 2.615, 3.490, 7.790)
  icer_output_tab3$delta.c <-
    c(0, 5777, 3200, 12706, 3000, 12571, 10000, 11000)
  icer_output_tab3$dominance <-
    c(rep("Not checked", nrow(icer_output_tab3)))
  icer_output_tab3$icer <-
    c(NA, 25905.82960, 7804.87805, 20626.62338, 2325.58140, 4807.26577,
      2865.32951, 1412.06675)
  icer_output_tab3$icer_label <-
    c("ICER(s) reference",
      rep("ICER reference is intervention 1", nrow(icer_output_tab3) - 1))

  icer_output_tab4 = icer_test_input2 %>%
    arrange(qalys) %>%
    mutate(delta.e = c(0.000, 0.077),
           delta.c = c(0, -5277),
           icer = c(NA, -68532.468),
           dominance = NA_character_,
           icer_label = NA_character_) %>%
    mutate(dominance = c(rep("Not checked", nrow(icer_test_input2))),
           icer_label = c("ICER(s) reference",
                          "ICER reference is intervention 2"))

  # Tests:
  # Full incremental analysis (computing ICERs accounting for dominance):
  # Several comparators (more than two interventions):
  expect_equal(icer_test_input1 %>%
                 compute_ICERs(),
               icer_output_tab1)
  # One comparator (two interventions):
  expect_equal(icer_test_input2 %>%
                 compute_ICERs(),
               icer_output_tab2)
  # Computing ICERs without dominance dominance consideration:
  # Several comparators (more than two interventions):
  expect_equal(icer_test_input1 %>%
                 compute_ICERs(.incremental = FALSE),
               icer_output_tab3)
  # One comparator (two interventions):
  expect_equal(icer_test_input2 %>%
                 compute_ICERs(.incremental = FALSE, .ref = 2),
               icer_output_tab4)
})
