test_that("rollup works", {
   rollup1 <- mtcars %>%
     dplyr::group_by(cyl) %>%
     rollup(mean = mean(disp), n = dplyr::n())

   expect_equal(rollup1$mean[[4]], mean(mtcars$disp))

   rollup2 <- mtcars %>%
     mutate(across(c(cyl, gear), as.character)) %>%
     dplyr::group_by(cyl, gear) %>%
     rollup(mean = mean(disp), n = dplyr::n())

   expect_equal(nrow(rollup2), 9)
   expect_equal(rollup2$cyl[[9]], rollup2$gear[[9]])
   expect_equal(rollup2$cyl[[9]], "Total")

   # Set label = NA to not label the last row (same as if it's a number)
   rollup3 <- mtcars %>%
     mutate(cyl = as.character(cyl)) %>%
     group_by(cyl) %>%
     rollup(sum = sum(disp),
            mean = mean(disp),
            sd = sd(disp),
            n = n(),
            label = NA)

   expect_equal(rollup3$cyl, c("4", "6", "8", NA_character_))
})
