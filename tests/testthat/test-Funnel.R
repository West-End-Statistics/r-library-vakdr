

devtools::load_all()

lg <- Funnel$new(mtcars)


mtcars %>%
  lg$filter(cyl > 6) %>%
  lg$filter(mpg > 15, gear == 3)


lg$show_log()
