#' UI module to arbitrary lines
#'
#' UI module to input either horizontal or vertical lines to a plot via comma separated values
#'
#' @param id (`character(1)`)\cr
#'  defining namespace of the `shiny` module.
#' @param line_arb (`numeric`)\cr
#'  default values for the `textInput` defining values of arbitrary lines
#' @param line_arb_color (`character`)\cr
#'  default values for the `textInput` defining colors of arbitrary lines
#' @param line_arb_label (`character`)\cr
#'  default values for the `textInput` defining labels of arbitrary lines
#' @param title (`character(1)`)\cr
#'  title of the arbitrary lines input. The default is "Arbitrary Horizontal Lines".
#' @return (`shiny.tag`) an input to define values, colors and labels for arbitrary
#' straight lines.
#' @keywords internal
ui_arbitrary_lines <- function(id, line_arb, line_arb_label, line_arb_color, title = "Arbitrary Horizontal Lines:") {
  ns <- NS(id)
  div(
    tags$b(title),
    textInput(
      ns("line_arb"),
      div(
        class = "teal-tooltip",
        tagList(
          "Value:",
          icon("circle-info"),
          span(
            class = "tooltiptext",
            "For multiple lines, supply a comma separated list of values."
          )
        )
      ),
      value = paste(line_arb, collapse = ", ")
    ),
    textInput(ns("line_arb_label"), label = "Label:", value = paste(line_arb_label, collapse = ", ")),
    textInput(ns("line_arb_color"), label = "Color:", value = paste(line_arb_color, collapse = ", "))
  )
}
#' Server module to arbitrary lines
#'
#' Server to validate and transform the comma separated values into vectors of values
#' to be passed into goshawk functions.
#' @inheritParams shiny::moduleServer
#' @return (`reactive`) returning a `list` containing `line_arb`, `line_arb_color`,
#'  `line_arb_label` which are validated and could be passed to `goshawk` plot functions.
#' @keywords internal
srv_arbitrary_lines <- function(id) {
  moduleServer(id, function(input, output, session) {

    comma_sep_to_values <- function(values, wrapper_fun = trimws, suppress_warnings = FALSE) {
      vals <- strsplit(values, "\\s{0,},\\s{0,}")[[1]]
      if (suppress_warnings) suppressWarnings(wrapper_fun(vals)) else wrapper_fun(vals)
    }

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("line_arb", shinyvalidate::sv_optional())
      iv$add_rule(
        "line_arb",
        ~ if (all(is.na(comma_sep_to_values(., as.numeric, TRUE))))
          "Arbitrary lines values should be a comma separate list of numbers"
      )
      iv
    })

    line_arb <- reactive({
      req(!is.null(input$line_arb))
      comma_sep_to_values(input$line_arb, as.numeric, TRUE)
    })

    line_arb_label <- reactive({
      if (length(line_arb()) == 0) {
        return(character(0))
      }
      val <- comma_sep_to_values(input$line_arb_label, suppress_warnings = FALSE)
      if (length(val) == 0) {
        val <- ""
      }
      val
    })

    line_arb_color <- reactive({
      if (length(line_arb()) == 0) {
        return(character(0))
      }
      val <- comma_sep_to_values(input$line_arb_color, suppress_warnings = FALSE)
      if (length(val) == 0 || all(val == "")) {
        val <- "red"
      }
      val
    })

    return(
      reactive(
        list(
          iv_r = iv_r,
          line_arb = line_arb(),
          line_arb_color = line_arb_color(),
          line_arb_label = line_arb_label()
        )
      )
    )


    # reactive({
    #   req(!is.null(input$line_arb), !is.null(input$line_arb_label), !is.null(input$line_arb_color))
    #   line_arb <- strsplit(input$line_arb, "\\s{0,},\\s{0,}")[[1]] %>%
    #     as.numeric()
    #   if ((length(line_arb) == 1 && is.na(line_arb)) || length(line_arb) == 0) {
    #     line_arb <- numeric(0)
    #     line_arb_label <- character(0)
    #     line_arb_color <- character(0)
    #   } else {
    #     validate(need(all(!is.na(line_arb)), "Invalid arbitrary line values"))
    #
    #     line_arb_label <- strsplit(input$line_arb_label, "\\s{0,},\\s{0,}")[[1]] %>%
    #       trimws()
    #     line_arb_color <- strsplit(input$line_arb_color, "\\s{0,},\\s{0,}")[[1]] %>%
    #       trimws()
    #     if (length(line_arb_label) == 0) {
    #       line_arb_label <- ""
    #     } else {
    #       validate(
    #         need(
    #           length(line_arb_label) %in% c(1, length(line_arb)),
    #           "Line input error: number of labels should be equal to 1 or the number of values"
    #         )
    #       )
    #     }
    #     if (length(line_arb_color) == 0 || all(line_arb_color == "")) {
    #       line_arb_color <- "red"
    #     } else {
    #       validate(
    #         need(
    #           length(line_arb_color) %in% c(1, length(line_arb)),
    #           "Line input error: number of colors should be equal to 1 or the number of values"
    #         )
    #       )
    #     }
    #   }
    #   list(line_arb = line_arb, line_arb_label = line_arb_label, line_arb_color = line_arb_color, iv_r = iv_r)
    # })
  })
}


# to check the arbitrary line arguments
validate_line_arb_arg <- function(line_arb, line_arb_color, line_arb_label) {
  checkmate::assert_numeric(line_arb)
  if (length(line_arb) > 0) {
    checkmate::assert(
      checkmate::check_string(line_arb_color),
      checkmate::check_character(line_arb_color, len = length(line_arb))
    )
    checkmate::assert(
      checkmate::check_string(line_arb_label),
      checkmate::check_character(line_arb_label, len = length(line_arb))
    )
  }
}

# to check the variable line arguments
validate_line_vars_arg <- function(line_vars, line_vars_colors, line_vars_labels) {
  checkmate::assert_character(line_vars)
  if (length(line_vars) > 0) {
    checkmate::assert(
      checkmate::check_string(line_vars_colors),
      checkmate::check_character(line_vars_colors, len = length(line_vars))
    )
    checkmate::assert(
      checkmate::check_string(line_vars_labels),
      checkmate::check_character(line_vars_labels, len = length(line_vars))
    )
  }
}
