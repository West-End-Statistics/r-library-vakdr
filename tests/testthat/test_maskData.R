context("Mask Data Function")

test_that("small number mask works", {

  total_surgeons <- 5
  test_df <- data.frame(original = sample(paste0("SURG",1:total_surgeons),30,
                                          replace = TRUE),
                        stringsAsFactors = FALSE)

  test_df <- dplyr::mutate(test_df, new = maskData(original))
  expect_equal(names(test_df$new), test_df$original)

  expect_setequal(test_df$new, LETTERS[1:5])
})

test_that("large number mask works", {
  skip_on_cran()
  total_surgeons <- 26^4 + 1

  old <- as.character(total_surgeons:1)
  new <- maskData(old, randomize = FALSE)

  expect_equal(names(new), old)
  expect_equal(unname(new[1]), "1", )
})
