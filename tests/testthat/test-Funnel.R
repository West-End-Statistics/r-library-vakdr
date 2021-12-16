


test_that("starting off", {
  skip("not ready")
  lg <- Funnel$new(mtcars)


  gear_cutoff <- 3


  mtcars %>%
    lg$filter(cyl > 6) %>%
    lg$filter(mpg > 15, gear == gear_cutoff)


  lg$show_log()

  fig <- lg$make_funnel_plot()

  save_funnel_as_image(fig, vheight = 300, zoom = 3)





  lg <- Funnel$new(mtcars, group_col = "cyl", unique_col = "disp")


  gear_cutoff <- 3


  mtcars %>%
    lg$filter(cyl > 6) %>%
    lg$filter(mpg > 15, gear == gear_cutoff)


  lg$show_log()

  fig <- lg$make_funnel_plot()

  save_funnel_as_image(fig, vheight = 300, zoom = 3)



  lg <- Funnel$new(mtcars)
})
