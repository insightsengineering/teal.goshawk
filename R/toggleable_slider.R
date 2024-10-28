#' UI with a toggleable dichotomous slider to change between slider and numeric input fields
#'
#' This is useful when a slider should be shown, but it is sometimes hard to configure sliders,
#' so one can toggle to one or two numeric input fields to set slider instead.
#' The toggle button will show two numeric input field for selecting the from and to range.
#'
#' @md
#' @param id `character` module id
#' @param label `label` label for input field, e.g. slider or numeric inputs
#' @param ... additional parameters to pass to `sliderInput`
#'
#' @name toggle_slider
#' @keywords internal
#' @return `NULL`.
NULL


#' @rdname toggle_slider
toggle_slider_ui <- function(id, label) {
  ns <- NS(id)
  tags$div(
    tags$div(
      style = "display: flex; justify-content: space-between;",
      tags$span(tags$strong(label)),
      tags$div(actionButton(ns("toggle"), "Toggle", class = "btn-xs"))
    ),
    uiOutput(ns("slider_view")),
    shinyjs::hidden(
      tags$div(
        id = ns("numeric_view"),
        numericInput(
          ns("value_low"),
          "From:",
          value = 0
        ),
        numericInput(
          ns("value_high"),
          "- to:",
          value = 0
        )
      )
    )
  )
}

#' @keywords internal
#' @rdname toggle_slider
toggle_slider_server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      min = NULL,
      max = NULL,
      value = NULL,
      step = NULL,
      slider = NULL,
      data_range = NULL
    )
    slider_shown <- reactive(input$toggle %% 2 == 0)

    observeEvent(state$data_range, {
      cat("state$date_range changed\n")
      state$min <- state$data_range[1]
      state$max <- state$data_range[2]
      state$value <- state$data_range
      state$step <- NULL # TODO
    })

    output$slider_view <- renderUI({
      cat("renderUI triggered\n")
      req(state$value)
      tags$div(
        class = "teal-goshawk toggle-slider-container",
        sliderInput(
          inputId = session$ns("slider"),
          label = NULL,
          min = min(state$data_range[1], state$min),
          max = max(state$data_range[2], state$max),
          value = state$value,
          step = state$step,
          ticks = TRUE,
          ...
        ),
        tags$script(HTML(sprintf(
          '
          $(".teal-goshawk.toggle-slider-container #%s").ready(function () {
            var tickLabel = document.querySelector(
              ".teal-goshawk.toggle-slider-container .irs-grid-text.js-grid-text-9"
            );
            var tick = document.querySelector(
              ".teal-goshawk.toggle-slider-container .irs-grid-pol:nth-last-child(6)"
            );
            if (tickLabel) {
              if (parseFloat(tickLabel.style.left) > 95) {
                tickLabel.style.opacity = "0";
                tick.style.opacity = "0";
              }
            } else {
              console.log("Toggle slider element not found.");
            }
          });
        ',
          session$ns("slider")
        )))
      )
    })

    observeEvent(input$toggle, {
      shinyjs::toggle("slider_view", condition = slider_shown())
      shinyjs::toggle("numeric_view", condition = !slider_shown())
    })

    observeEvent(input$slider, {
      state$value <- input$slider
    })

    observeEvent(state$value, { # todo: change to state$value
      cat("state$value changed\n")
      if (!setequal(state$value, c(input$value_low, input$value_high))) {
        cat("state differs from input updating numeric input\n")
        updateNumericInput(session, "value_low", value = state$value[1])
        updateNumericInput(session, "value_high", value = state$value[2])
      }
    })

    observeEvent(c(input$value_low, input$value_high), ignoreInit = TRUE, {
      cat("input$numeric changed - updating state value\n")
      values <- c(input$value_low, input$value_high)
      if (all(!is.na(values))) {
        state$value <- values
      }
    })

    return(state)
  })
}

#' @keywords internal
#' @rdname toggle_slider
keep_slider_state_updated <- function(state, varname, paramname, ANL, trt_group = NULL, step = NULL) { # nolint object_name_linter
  validate(need(varname, "Please select variable"))
  validate(need(paramname, "Please select variable"))
  req(length(paramname) == 1)

  ANL <- ANL %>% dplyr::filter(.data$PARAMCD == paramname) # nolint object_name_linter
  validate_has_variable(ANL, varname, paste("variable", varname, "does not exist"))

  var <- stats::na.omit(ANL[[varname]])
  minmax <- if (length(var)) c(floor(min(var)), ceiling(max(var))) else c(0, 0)
  if (!is.null(trt_group)) {
    ANL_split <- ANL %>% split(f = factor(paste0(ANL[["AVISITCD"]], ANL[[trt_group]]))) # nolint
    density_maxes <- lapply(ANL_split, function(x) {
      max(stats::density(stats::na.omit(x[[varname]]))$y)
    })
    dmax <- max(unlist(density_maxes))
    minmax <- c(0, round(dmax * 1.2, 5))
    step <- round(dmax / 100, 5)
  }
  state$data_range <- c(min = minmax[[1]], max = minmax[[2]])
  state
}
