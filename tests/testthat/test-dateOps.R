test_that("personAge works", {
  birth_date <- as.Date('1978-02-22')

  expect_equal(personAge(birth_date, '2021-02-01'), 42)
  expect_equal(personAge(birth_date, '2021-02-21'), 42)
  expect_equal(personAge(birth_date, '2021-02-22'), 43)
  expect_equal(personAge(birth_date, '2021-03-22'), 43)
})


test_that("pretty_date works", {
  date1 <- as.Date('2020-08-11')

  expect_equal(pretty_date(date1), "August 11th, 2020")
  expect_equal(pretty_date(date1, month_code = "%b."),
               "Aug. 11th, 2020")

  # error when non-date passed
  expect_error(pretty_date('2021-08-11'))
})
