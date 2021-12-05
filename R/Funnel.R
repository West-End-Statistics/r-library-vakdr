#' @export
Funnel <- R6::R6Class(
  "Funnel",
  public = list(
    name = character(),
    unique_col = NA_character_,
    group_col = NA_character_,
    cur_step = 1,
    nongroup_label = NA_character_,
    initialize = function(data, unique_col = NA_character_, group_col = NA_character_,
                          starting_label = "Full data set",
                          nongroup_label = "dataframe"
                          ) {
      self$name <- rlang::expr_label(substitute(data))
      self$group_col <- group_col
      self$unique_col <- unique_col
      self$nongroup_label <- nongroup_label

      # starting_rows <- nrow(data)

      private$snapshot_data(data, label = starting_label)
    },
    filter = function(.data, ..., .preserve = FALSE, .label = NULL) {

      out <- dplyr::filter(.data = .data, ..., .preserve = .preserve)

      self$cur_step <- self$cur_step + 1

      if(is.null(.label)) {
        .label  <- paste(sapply(rlang::enquos(...), rlang::as_label), collapse = ", ")
      }

      private$snapshot_data(out, label = .label)

      out
    },
    make_funnel_plot = function(label_suffix = ":") {

      if(requireNamespace("plotly", quietly = TRUE)) {
        funnel_def <- private$make_funnel_def()
        names(funnel_def) <- paste0(names(funnel_def), label_suffix)

        fig <- plotly::plot_ly()
        fig <- plotly::add_trace(
            fig,
            type = "funnel",
            y = names(funnel_def),
            x = funnel_def,
            texttemplate = "%{value:,f}",
            textinfo = "value+percent previous")
        fig <- plotly::layout(fig,
                      yaxis = list(categoryarray = names(funnel_def)))
      } else {
        stop("The plotly package is required to create funnel plots. Please install with:\n",
             "    install.packages('plotly')")

      }

      fig
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
      unique = integer(),
      label = character()
    ),
    make_funnel_def = function() {
      out <- private$log$count
      names(out) <- private$log$label
      out
    },
    snapshot_data = function(data, label) {
      private$add_log_entry(
        private$create_log_entry(data,
                                 label = label,
                                 self$unique_col,
                                 group_col = NA_character_)
      )


      if(!is.na(self$group_col)) {
        private$add_log_entry(
          private$create_log_entry(data,
                                   label = label,
                                   self$unique_col,
                                   group_col = self$group_col)
        )
      }

      self

    },
    create_log_entry = function(data,
                                 label,
                                 group_col = NA_character_,
                                 unique_col = NA_character_) {

      if(is.na(unique_col)) {
        # setting to NULL means that summarise across will automatically ignore it
        unique_col <- NULL
      }

      cur_step <- self$cur_step
      nongroup_label = self$nongroup_label

      data <- ungroup(data)
      if(!is.na(group_col)) {
        data <- dplyr::group_by(data, across(all_of(group_col)))
      }

      counts <- dplyr::summarise(data,
                                 count = n(),
                                 across(all_of(unique_col), n_distinct, .names = "unique"),

                                 .groups = "drop")
      if(is.na(group_col)) {
        # a column needs to be added if it's not grouped by anything
        counts <- tibble::add_column(counts, unit = nongroup_label, .before = 1)
      }else {
        colnames(counts)[1] <- "unit"
        counts[["unit"]] <- as.character(counts[["unit"]])
      }

      counts[["step"]] <- self$cur_step
      counts[["label"]] <- label

      counts

    },

    add_log_entry = function(new_entry) {
      private$log <- dplyr::bind_rows(
        private$log,
        new_entry
      )

      self
    }
  ))

#' @export
save_funnel_as_image <- function(fig, file = "funnel.png", vwidth = 450, vheight = 450, zoom = 2) {
  if(!requireNamespace("webshot", quietly = TRUE) || !requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("The webshot and htmlwidgets packages are required to create pngs from widgets. Please install with:\n",
         "    install.packages('webshot', 'htmlwidgets')\n",
         "You must also have PhantomJS installation. This can be installed with:\n",
         "    webshot::install_phantomjs()")

  }

  if(!webshot::is_phantomjs_installed()) {
    stop("You must also have PhantomJS installation. This can be installed with:\n",
         "    webshot::install_phantomjs()")
  }

  temp_html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(fig, temp_html)
  webshot::webshot(temp_html, file = file, vwidth = vwidth, vheight = vheight, zoom = zoom)
}
