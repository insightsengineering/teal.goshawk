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
ui_arbitrary_lines <- function(id, line_arb, line_arb_label, line_arb_color, title = "Arbitrary horizontal lines:") {
  ns <- NS(id)
  tags$div(
    tags$b(
      title,
      bslib::tooltip(
        trigger = icon("circle-info"),
        tags$span(
          "For multiple lines, supply a comma separated list of values."
        )
      )
    ),
    textInput(
      ns("line_arb"),
      "Value:",
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
    comma_sep_to_values <- function(values, wrapper_fun = trimws) {
      vals <- strsplit(values, "\\s{0,},\\s{0,}")[[1]]
      suppressWarnings(wrapper_fun(vals))
    }

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("line_arb", shinyvalidate::sv_optional())
      iv$add_rule(
        "line_arb",
        ~ if (any(is.na(comma_sep_to_values(., as.numeric)))) {
          "Arbitrary lines values should be a comma separated list of numbers"
        }
      )

      iv_color <- shinyvalidate::InputValidator$new()
      iv_color$condition(~ length(line_arb()) != 0)

      iv_color$add_rule("line_arb_color", shinyvalidate::sv_optional())
      iv_color$add_rule(
        "line_arb_color",
        ~ if (!length(comma_sep_to_values(.)) %in% c(1, length(line_arb()))) {
          sprintf(
            "Line input error: number of colors should be equal to 1, the number of lines (%d) or left blank for 'red'",
            length(line_arb())
          )
        }
      )
      iv_color$add_rule("line_arb_color", ~ if (!check_color(comma_sep_to_values(.))) {
        "The line colors entered cannot be converted to colors in R, please check your spelling"
      })
      iv$add_validator(iv_color)


      iv_label <- shinyvalidate::InputValidator$new()
      iv_label$condition(~ length(line_arb()) != 0)

      iv_label$add_rule("line_arb_label", shinyvalidate::sv_optional())
      iv_label$add_rule(
        "line_arb_label",
        ~ if (!length(comma_sep_to_values(.)) %in% c(1, length(line_arb()))) {
          sprintf(
            "Line input error: number of labels should be equal to 1, the number of lines (%d) or left blank",
            length(line_arb())
          )
        }
      )
      iv$add_validator(iv_label)
      iv
    })

    line_arb <- reactive({
      req(!is.null(input$line_arb))
      comma_sep_to_values(input$line_arb, as.numeric)
    })

    line_arb_label <- reactive({
      if (length(line_arb()) == 0) {
        return(character(0))
      }
      val <- comma_sep_to_values(input$line_arb_label)
      if (length(val) == 0) {
        val <- ""
      }
      val
    })

    line_arb_color <- reactive({
      if (length(line_arb()) == 0) {
        return(character(0))
      }
      val <- comma_sep_to_values(input$line_arb_color)
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
  })
}

check_color <- function(col) {
  tryCatch(
    is.matrix(grDevices::col2rgb(col)),
    error = function(e) FALSE
  )
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
