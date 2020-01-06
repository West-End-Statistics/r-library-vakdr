context("Make sure writeXLSX is working")

test_that("writeXLSX is able to create a file", {
  tmp_file_loc <- tempfile(fileext = ".xlsx")
  old_disp_var <- Sys.getenv("DISPLAY")

  try(writeXLSX(mtcars, file = tmp_file_loc))
  expect_true(file.exists(tmp_file_loc))

  ## Check to make sure old display variable is set again
  expect_equal(old_disp_var, Sys.getenv("DISPLAY"))

  # clean up temporary file
  file.remove(tmp_file_loc)
})




test_that("writeXLSX works from a cron job",{
  skip_if(Sys.which('crontab')=="", message = "crontab not installed on system")
  tmp_file_R <- tempfile(fileext = ".R")


  tmp_file_out <- tempfile(fileext = ".xlsx")

  script <- glue::glue("vakdr::writeXLSX(mtcars, file = '{tmp_file_out}')")


  writeLines(script, con = tmp_file_R)


  library(cronR)


  cron_add(cron_rscript(tmp_file_R), frequency = 'minutely', id = "test_job")

  max_wait <- 60

  start <- Sys.time()

  wait_time <- as.numeric(Sys.time() - start, unit = "secs")
  while (wait_time < max_wait & !file.exists(tmp_file_out)) {
    Sys.sleep(1)
    wait_time <-as.numeric(Sys.time() - start, unit = "secs")
  }

  expect_true(file.exists(tmp_file_out))
  cron_rm(id = "test_job")
}
)
