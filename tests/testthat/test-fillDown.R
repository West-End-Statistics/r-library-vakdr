test_that("fill down works", {
  set.seed(2)
  d <- data.frame(meas = rnorm(100),
                   time = 1:100)
  d$meas[sample(1:nrow(d), 70)] <- NA

  main_out <- fillDown(d$meas, d$time, 3)


  expect_equal(unique(main_out[81:84]), d$meas[81])
  expect_equal(main_out[85], NA_real_)

  expect_equal(d$meas[!is.na(d$meas)],
               main_out[!is.na(d$meas)]
  )

})
