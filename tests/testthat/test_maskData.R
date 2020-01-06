context("Mask Data Function")

if(interactive()){
  library(testthat)
}

total_surgeons <- 5
test_df <- data.frame(original = sample(paste0("SURG",1:total_surgeons),30,
                                        replace = TRUE),
                      stringsAsFactors = FALSE)

test_df <- dplyr::mutate(test_df, new = maskData(original))

expect_equal(names(test_df$new), test_df$original)

expect_error(maskData(1:700))
