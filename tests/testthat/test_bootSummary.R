test_that("bootSummary is working",{
  set.seed(2)

  ungrouped_df <- bootSummary(mtcars, wt)
  ungrouped_df_sum <- bootSummary(mtcars, wt, .funs = sum)
  grouped_df <- bootSummary(mtcars, wt, gear)

  expect_equal(names(ungrouped_df),
               c("stat_mean","stat_mid","stat_low" ,"stat_high"))
  expect_equal(nrow(ungrouped_df),1)

  expect_equal(names(grouped_df),
               c("gear", "stat_mean","stat_mid","stat_low" ,"stat_high"))

  expect_gte(ungrouped_df_sum$stat_mean, ungrouped_df$stat_mean)


})


