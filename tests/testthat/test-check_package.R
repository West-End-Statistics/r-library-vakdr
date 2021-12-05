test_that("check_package works", {
  expect_true(check_package('base'))


  expect_error(check_package('lkjfas'),
               regexp = "install.packages\\('lkjfas'\\)"
               )

  expect_error(check_package('lkjfas', msg = "We need this for xyz"),
               regexp = "xyz")

  expect_error(check_package('lkjfas', install_directions = "remotes::install_github",
               regexp = "remotes::install_github")
  )
})
