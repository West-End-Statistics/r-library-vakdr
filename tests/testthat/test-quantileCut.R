test_that("quantileCut works", {

  x <- 1:100

  out <- quantileCut(x)

  expect_length(levels(out), 10)

  x <- sample(1:6, size = 100, replace = TRUE)

  expect_warning(quantileCut(x, silent = FALSE))
  expect_error(quantileCut(x, silent = FALSE, force_unique = F))
  out <- quantileCut(x)
  expect_length(levels(out), 5)
})
