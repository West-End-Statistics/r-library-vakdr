context("window Threshold C++ Function")

test_that("look backward functions properly, no look forward", {
  n <- 10
  add_nas <- 2

  look_backward <- 3
  look_forward <- 0
  meas_norm <- c(-1,1)
  set.seed(23)
  meas_col <- rnorm(n)
  add_nas <- min(n, add_nas)
  meas_col[sample(1:n,add_nas)] <- NA
  time_col <- c(0, 3, 5, 8, 11, 15,18,19, 22.1, 28)
  expected_output <- c(FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE)
  expect_equal(windowThreshold(meas_col, meas_norm , time_col, look_backward, look_forward),
               expected_output)
})

