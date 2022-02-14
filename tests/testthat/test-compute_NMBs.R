################################################################################
#
# Script Name:        test-compute_NMBs.R
# Module Name:        Economic/PSA
# Script Description: Tests functions defined in compute_NMBs.R
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

test_that("Compute NMBs", {
  # Define a dataset to test the function:
  tmp <- c(rep(1.5, 3), rep(2, 3), rep(1.75, 4))
  nmb_test_effs1 <- matrix(data = c(tmp, tmp * 1.1, tmp * 0.75),
                           ncol = 3, byrow = FALSE)
  nmb_test_effs2 <- matrix(data = c(tmp, tmp * 1.1),
                           ncol = 2, byrow = FALSE)
  nmb_test_effs3 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "testthat_input_effects1.rds"))
  tmp <- c(rep(500, 3), rep(530, 3), rep(760, 4))
  nmb_test_csts1 <- matrix(data = c(tmp, tmp * 1.1, tmp * 0.75),
                           ncol = 3, byrow = FALSE)
  nmb_test_csts2 <- matrix(data = c(tmp, tmp * 1.1),
                           ncol = 2, byrow = FALSE)
  nmb_test_csts3 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "testthat_input_costs1.rds"))

  # Define expected outputs:
  nmb_output_list1 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "nmb_output_test1.rds"))
  nmb_output_list2 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "nmb_output_test2.rds"))
  nmb_output_list3 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "nmb_output_test3.rds"))
  nmb_output_list4 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "nmb_output_test4.rds"))

  # Tests:
  # Compute NMB (without setting a reference intervention):
  expect_equal(compute_NMBs(.effs = nmb_test_effs1,
                            .costs = nmb_test_csts1),
               nmb_output_list1)
  expect_equal(compute_NMBs(.effs = nmb_test_effs2,
                            .costs = nmb_test_csts2),
               nmb_output_list3)
  expect_equal(compute_NMBs(.effs = nmb_test_effs3,
                            .costs = nmb_test_csts3),
               nmb_output_list4)
  # Compute iNMB (without setting a reference intervention):
  expect_equal(compute_NMBs(.effs = nmb_test_effs1,
                            .costs = nmb_test_csts1,
                            .ref = 2),
               nmb_output_list2)
})

test_that("Compute CEACs", {
  # Define a dataset to test the function:
  ceac_test_input <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "nmb_output_test4.rds"))
  ceac_test_effs <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "testthat_input_effects1.rds"))
  ceac_test_csts <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "testthat_input_costs1.rds"))

  # Define expected outputs:
  ceac_output_test1 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "ceac_output_test1.rds"))
  ceac_output_test2 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "ceac_output_test2.rds"))

  # Tests:
  # Compute CEACs from NMB:
  expect_equal(compute_CEACs(.nmb = ceac_test_input$nmb),
               ceac_output_test1)
  # Compute CEACs from PSA costs and effects matrices:
  expect_equal(compute_CEACs(.nmb = NULL,
                             .effs = ceac_test_effs,
                             .costs = ceac_test_csts),
               ceac_output_test1)
  # Compute CEACs from PSA costs and effects but setting a reference option:
  expect_equal(compute_CEACs(.nmb = NULL,
                             .effs = ceac_test_effs,
                             .costs = ceac_test_csts,
                             .ref = 1),
               ceac_output_test2)
})

test_that("Compute CEAFs", {
  # Define a dataset to test the function:
  ceaf_test_input1 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "ceac_output_test1.rds"))
  ceaf_test_input2 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "nmb_output_test4.rds"))

  # Define expected outputs:
  ceaf_output_test1 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "ceaf_output_test1.rds"))
  ceaf_output_test2 <- readr::read_rds(file = file.path(here::here(), "tests", "testthat", "testdata", "ceaf_output_test2.rds"))

  # Tests:
  # Compute CEAFs from CEAC:
  expect_equal(compute_CEAFs(.ceac = ceaf_test_input1),
               ceaf_output_test1)
  # Compute CEAFs from NMB:
  expect_equal(compute_CEAFs(.ceac = NULL, .nmb = ceaf_test_input2$nmb),
               ceaf_output_test2)
})
