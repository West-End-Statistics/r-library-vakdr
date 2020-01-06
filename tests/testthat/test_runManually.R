test_that("runLocalScript works",{
  # create a file to run
  script_to_run <- tempfile(fileext = ".R")
  writeLines("2 + 2", con = script_to_run)
  correct_run <- runLocalScript(script_to_run)
  expect_equal(correct_run$status, 0)
  # create a file with an error
  writeLines("2 + 'a'", con = script_to_run)
  error_run <- runLocalScript(script_to_run)
  expect_equal(error_run$status, 1)

  expect_error(runLocalScript("thisisnotafile.R", error_on_missing = TRUE))
  expect_equal(runLocalScript("thisisnotafile.R")$status, 1)


}


)
