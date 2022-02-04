test_that("Identifying the maximum value by row in a data frame", {
  # Define a dataset to test the function:
  test_data = matrix(data = c(seq(10, 1000, 10), 1:50), nrow = 10, byrow = TRUE)

  # Define expected outputs:
  ouptut_data = c(150, 300, 450, 600, 750, 900, 1000, 20, 35, 50)

  # Test:
  expect_equal(get_row_max(test_data),
               ouptut_data)
})
