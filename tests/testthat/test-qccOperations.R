test_that("qcc augment works", {
  skip_if_not_installed('qcc')
  data("pistonrings", package = 'qcc')
  diameter <- qcc::qcc.groups(pistonrings$diameter, pistonrings$sample)
  qcc_obj <- qcc::qcc(diameter[1:25,], type="xbar", plot = FALSE)
  aug1 <- augment(qcc_obj)

  expect_equal(dim(aug1), c(25, 9))
  expect_true(all(aug1$violations == 'None'))


  qcc_new <-  qcc::qcc(diameter[1:25,],
                       newdata = diameter[26:40,],
                       type="xbar", plot = FALSE)
  aug2 <- augment(qcc_new)

  expect_equal(
    qcc_new$violations$beyond.limits,
    which(aug2$violations == 'Beyond Limits')
  )

  expect_equal(
    qcc_new$violations$violating.runs,
    which(aug2$violations == 'Violating Run')
  )

})

test_that("qcc formula works", {

  skip_if_not_installed('qcc')

  data("pistonrings", package = 'qcc')
  diameter <- qcc::qcc.groups(pistonrings$diameter, pistonrings$sample)

  qcc_obj <- qcc::qcc(diameter[1:25,], type="xbar", plot = FALSE)
  qcc_obj2 <- qcc.formula(diameter ~ sample,
                          data = dplyr::filter(pistonrings, sample <= 25),
                          type = "xbar", plot = FALSE)

  # don't look at call or data.name
  items_to_compare <- c("type", "data", "statistics", "sizes",
                        "center", "std.dev", "nsigmas", "limits", "violations")

  expect_identical(qcc_obj[items_to_compare], qcc_obj2[items_to_compare])

  qcc_obj <-  qcc::qcc(diameter[1:25,], newdata = diameter[26:40,], type="xbar", plot = FALSE)
  qcc_obj2 <- qcc.formula(diameter ~ sample,
                          data = pistonrings,
                          type = "xbar", plot = FALSE, cutoff = 26)

  expect_identical(qcc_obj[items_to_compare], qcc_obj2[items_to_compare])


})
