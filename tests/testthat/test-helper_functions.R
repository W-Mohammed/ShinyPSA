################################################################################
#
# Script Name:        test-icer_helper_functions.R
# Module Name:        Economic/PSA
# Script Description: Tests the set of helper functions defined in
#                     get_icers.R.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

test_that("Identifying the maximum value by row in a data frame", {
  # Define a dataset to test the function:
  test_data = matrix(data = c(seq(10, 1000, 10), 1:50), nrow = 10, byrow = TRUE)

  # Define expected outputs:
  ouptut_data = c(150, 300, 450, 600, 750, 900, 1000, 20, 35, 50)

  # Test:
  expect_equal(get_row_max(test_data),
               ouptut_data)
})


test_that("Missing columns are added accordingly", {
  # Define a dataset to test the function:
  test_data = tibble()

  # Define expected outputs:
  ouptut_data = tibble(
    delta.e = vector(mode = "double"),
    delta.c = vector(mode = "double"),
    icer = vector(mode = "double"),
    dominance = vector(mode = "character"),
    icer_label = vector(mode = "character")
  )

  # Test:
  expect_equal(test_data %>%
                 add_missing_columns(
                   .characters = c("dominance", "icer_label"),
                   .numerics = c("delta.e", "delta.c", "icer")),
               ouptut_data)
})
