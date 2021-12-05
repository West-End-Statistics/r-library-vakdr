test_that("lkp_to_factor", {

  test_vect <- c("ok", "ok", "nice", "lol", "pop")


  expect_equal(
    levels(lkp_to_factor(test_vect,
                         c("ok", "nice", "pop", "lol"))),
    c("ok", "nice", "pop", "lol")
  )

  expect_error(lkp_to_factor(test_vect,
                             c("ok", "pop", "lol"))
  )

  expect_equal(
    levels(lkp_to_factor(test_vect,
                         c("ok", "pop", "lol"), check_names = FALSE)),
    c("ok", "pop", "lol")
  )


  expect_equal(
  levels(lkp_to_factor(test_vect,
                c("ok" = "lol", "nice" = "name", "pop"= "pop", "lol" = "lol"))
  ),
  c("lol", "name", "pop")

  )

  expect_equal(
    lkp_to_factor(test_vect,
                  c("ok" = "lol", "nice" = "name", "pop"= "pop", "lol" = "lol")),
    factor(c("lol", "lol", "name", "lol", "pop"),
           levels = c("lol", "name", "pop"))

  )

  expect_equal(
    lkp_to_factor(as.factor(test_vect),
                  c("ok" = "lol", "nice" = "name", "pop"= "pop", "lol" = "lol")),
    factor(c("lol", "lol", "name", "lol", "pop"),
           levels = c("lol", "name", "pop"))

  )
})
