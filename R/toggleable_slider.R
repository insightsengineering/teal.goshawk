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
    uiOutput(ns("inputs"))
  )
}

#' @keywords internal
#' @rdname toggle_slider
toggle_slider_server <- function(id, data_state, ...) {
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      min = NULL,
      max = NULL,
      value = NULL
    )
    slider_shown <- reactive(input$toggle %% 2 == 0)

    observeEvent(data_state()$range, {
      state$min <- data_state()$range[1]
      state$max <- data_state()$range[2]
      state$value <- data_state()$range
    })

    output$inputs <- renderUI({
      req(state$value)
      if (slider_shown()) {
        tags$div(
          class = "teal-goshawk toggle-slider-container",
          sliderInput(
            inputId = session$ns("slider"),
            label = NULL,
            min = min(data_state()$range[1], state$min),
            max = max(data_state()$range[2], state$max),
            value = state$value,
            step = data_state()$step,
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
      } else {
        tags$div(
          class = "teal-goshawk toggle-slider-container",
          numericInput(
            inputId = session$ns("value_low"),
            label = "From:",
            value = state$value[1]
          ),
          numericInput(
            inputId = session$ns("value_high"),
            label = "to:",
            value = state$value[2]
          )
        )
      }
    })

    d_slider <- debounce(reactive(input$slider), 500)

    observeEvent(d_slider(), {
      if (!setequal(state$value, d_slider())) {
        state$value <- d_slider()
      }
    })

    d_value_low <- debounce(reactive(input$value_low), 500)
    d_value_high <- debounce(reactive(input$value_high), 500)

    observeEvent(c(d_value_low(), d_value_high()), ignoreInit = TRUE, {
      values <- c(input$value_low, input$value_high)
      if (!setequal(state$value, values)) {
        state$value <- values
        state$min <- values[1]
        state$max <- values[2]
      }
    })

    return(state)
  })
}

#' @keywords internal
#' @rdname toggle_slider
get_data_range_states <- function(varname, paramname, ANL, trt_group = NULL, step = NULL) { # nolint object_name_linter
  validate(need(varname, "Please select variable"))
  validate(need(paramname, "Please select variable"))
  req(length(paramname) == 1)
  step <- NULL

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
  list(
    range = c(min = minmax[[1]], max = minmax[[2]]),
    step = step
  )
}
