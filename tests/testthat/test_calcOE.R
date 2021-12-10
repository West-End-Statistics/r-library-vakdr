if(interactive()){
  library(testthat)
  `%>%` <- dplyr::`%>%`
  devtools::load_all()
}

set.seed(20181113)
df_size <- 100
d <- data.frame(groups = sample(c("A", "B"), df_size, replace = T),
                groups2 = sample(c("C", "D"), df_size, replace = T),
                groups3 = c("E", rep("F", df_size -1)),
                e = runif(df_size)) %>%
  mutate(o = purrr:::map_int(e, ~rbinom(1,1, .)),
         o2 = 0)

test_that("Formula interface is working correctly", {
  expect_error(calcOE( ~ e, data = d))
  expect_error(calcOE( e ~ 1, data = d))
  expect_error(calcOE(o ~ e + groups, data = d))
  expect_error(calcOE(o + o ~ 1, data = d))
  expect_error(calcOE(groups ~ 1, data = d))
  expect_is(calcOE(o ~ e, data = d), "data.frame")
})


test_that("Methods produce similar estimates when n is 100", {

  binconf_est <- calcOE(o ~ e, data = d, method = "binconf")
  pois_est <- calcOE(o ~ e, data = d, method = "poisson")
  boot_est <- calcOE(o ~ e, data = d, method = "bootstrap")

  expect_true(all(abs(binconf_est - pois_est) < .1))
  expect_true(all(abs(binconf_est - boot_est) < .1))

  binconf_est <- calcOE(o ~ 1, data = d, method = "binconf")
  pois_est <- calcOE(o ~ 1, data = d, method = "poisson")
  boot_est <- calcOE(o ~ 1, data = d, method = "bootstrap")

  # divide by 50 as that's the expected O
  expected_o <- nrow(d)/2
  expect_true(all(abs(binconf_est - pois_est)/expected_o < .1))
  expect_true(all(abs(binconf_est - boot_est)/expected_o < .1))
})


test_that("Works as expected with groups", {
  expect_equal(dim(calcOE(o ~ e, data = d, groups, groups2)), c(4,6))
  expect_equal(dim(calcOE(o ~ e, data = d, groups2)), c(2, 5))
  expect_equal(dim(calcOE(o2 ~ e, data = d)), c(1, 4))
})

test_that("Works with 1 case",{
  # if it does not work with 1 group then there will be NA's in the data.frame
  # and there will be 0 rows
  expect_equal(calcOE(o ~ e, data = d, groups3) %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    nrow(), 2)
})
