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
#' @param initial_state `reactiveValues` list with min, max, step, value and change_counter.
#' `initial_state` provides the initial state for the slider and the numeric inputs,
#' it can also help to reset the states from outside the shiny module.
#' Check the `keep_slider_state_updated` for a common way to reset the slider state.
#' min - The min range of the slider.
#' max - The max range of the slider.
#' step - The step size of the slider and numericInput.
#' value - The selected values of the slider.
#' change_counter - A counter to make sure that we also reset the slider even if the previous and current state is same.
#'
#' @examples
#'
#' # use non-exported function from teal.goshawk
#' toggle_slider_ui <- getFromNamespace("toggle_slider_ui", "teal.goshawk")
#' toggle_slider_server <- getFromNamespace("toggle_slider_server", "teal.goshawk")
#'
#' ui <- fluidPage(
#'   shinyjs::useShinyjs(),
#'   toggle_slider_ui(
#'     "toggle_slider", "Select value"
#'   ),
#'   verbatimTextOutput("value")
#' )
#'
#' server <- function(input, output, session) {
#'   init_state <- reactiveValues(min = 0, max = 10, value = c(3, 6), step = 0.5, change_counter = 0)
#'   range_value <- toggle_slider_server("toggle_slider", init_state)
#'   messages <- reactiveVal() #' to keep history
#'   observeEvent(range_value(), {
#'     list_with_names_str <- function(x) paste(names(x), x, sep = ": ", collapse = ", ")
#'     messages(c(messages(), list_with_names_str(range_value())))
#'   })
#'   output$value <- renderText({
#'     paste(messages(), collapse = "\n")
#'   })
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
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
toggle_slider_server <- function(id, initial_state, print = FALSE, ...) {
  moduleServer(id, function(input, output, session) {
    selected_state <- reactiveVal(NULL)
    slider_update_state <- reactiveVal(NULL)
    slider_shown <- reactive(input$toggle %% 2 == 0)

    observeEvent(initial_state$change_counter, {
      selected_state(
        list(
          min = initial_state$min,
          max = initial_state$max,
          step = initial_state$step,
          value = initial_state$value
        )
      )
      slider_update_state(
        list(
          min = initial_state$min,
          max = initial_state$max,
          step = initial_state$step,
          value = initial_state$value,
          change_counter = initial_state$change_counter
        )
      )
    })

    output$slider_view <- renderUI({
      req(slider_update_state())
      args <- list(
        inputId = session$ns("slider"),
        label = NULL,
        min = slider_update_state()$min,
        max = slider_update_state()$max,
        value = slider_update_state()$value,
        step = slider_update_state()$step,
        ...
      )
      if (length(seq(slider_update_state()$min, slider_update_state()$max)) < 10) {
        args$ticks <- TRUE
        html <- do.call("sliderInput", args)
      } else {
        html <- do.call("sliderInput", args)
      }
      tags$div(
        class = "teal-goshawk toggle-slider-container",
        html,
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
      if (slider_shown()) {
        selected_state(
          list(
            min = selected_state()$min,
            max = selected_state()$max,
            value = input$slider
          )
        )
      }
    })

    observeEvent(c(input$value_low, input$value_high), ignoreInit = TRUE, {
      if (!slider_shown()) {
        selected_state(
          list(
            min = min(initial_state$min, input$value_low),
            max = max(initial_state$max, input$value_high),
            value = c(input$value_low, input$value_high)
          )
        )
        slider_update_state(
          list(
            min = selected_state()$min,
            max = selected_state()$max,
            value = selected_state()$value
          )
        )
      }
    })

    observeEvent(selected_state(), {
      updateNumericInput(session, "value_low", value = selected_state()$value[1], step = selected_state()$step)
      updateNumericInput(session, "value_high", value = selected_state()$value[2], step = selected_state()$step)
    })

    return(selected_state)
  })
}

#' @keywords internal
#' @rdname toggle_slider
keep_slider_state_updated <- function(intial_state, varname, paramname, ANL, trt_group = NULL, step = NULL) { # nolint object_name_linter
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
  intial_state$min <- minmax[[1]]
  intial_state$max <- minmax[[2]]
  intial_state$step <- step
  intial_state$value <- minmax
  intial_state$change_counter <- isolate(intial_state$change_counter) + 1
  intial_state
}
