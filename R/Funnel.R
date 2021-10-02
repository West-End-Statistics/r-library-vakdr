Funnel <- R6::R6Class(
  "Funnel",
  public = list(
    name = character(),
    default_unique = NA_character_,
    default_groups = NA_character_,

    initialize = function(data, default_unique = NA_character_, default_groups = NA_character_,
                          starting_label = "Full data set"

                          ) {
      self$name <- rlang::expr_label(substitute(data))

      starting_rows <- nrow(data)

      private$log <- tibble::add_row(
        private$log,
        step = 1,
        unit = "dataframe",
        count = starting_rows,
        label = starting_label
      )
    },
    filter = function(.data, ..., .preserve = FALSE, .label = NULL) {

      out <- dplyr::filter(.data = .data, ..., .preserve = .preserve)



      step_num <- max(private$log$step) + 1

      if(is.null(.label)) {
        .label = paste("Filter step", step_num - 1)
        # currently only cacptures first item in ...
        .label =   rlang::expr_label(substitute(...))
      }

      private$log <- tibble::add_row(
        private$log,
        step = step_num,
        unit = "dataframe",
        count = nrow(out),
        label = .label
      )
      out
    },
    show_log = function() {
      private$log
    }
  ),
  private = list(
    log = tibble::tibble(
      step = integer(),
      unit = character(),
      count = integer(),
      label = character()
    )



  ))
