################################################################################
#
# Script Name:        test-compute_EVPIs.R
# Module Name:        Economic/PSA
# Script Description: Tests functions defined in compute_EVPIs.R
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

test_that("Compute EVPIs", {
  # Define a dataset to test the function:
  evpi_test_effs <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "testthat_input_effects1.rds"))
  evpi_test_csts <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "testthat_input_costs1.rds"))

  # Define expected outputs:
  evpi_output_list <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "evpi_output_test.rds"))

  # Tests:
  expect_equal(compute_EVPIs(.effs = evpi_test_effs,
                             .costs = evpi_test_csts),
               evpi_output_list)
})
