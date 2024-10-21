#' UI with a toggleable slider to change between slider and numeric input fields
#'
#' This is useful when a slider should be shown, but it is sometimes hard to configure sliders,
#' so one can toggle to one or two numeric input fields to set slider instead.
#' Both normal sliders (for a single number in a range) and dichotomous sliders (for a range
#' within the slider range) are supported. In the former case, the toggle button
#' will show one numeric input field, in the latter case two.
#'
#' Value is not checked to be within minmax range
#'
#' @md
#' @param id `character` module id
#' @param label `label` label for input field, e.g. slider or numeric inputs
#'
#' @examples
#' value <- c(20.3, 81.5) # dichotomous slider
#' # value <- c(50.1) # normal slider
#'
#' # use non-exported function from teal.goshawk
#' toggle_slider_ui <- getFromNamespace("toggle_slider_ui", "teal.goshawk")
#' toggle_slider_server <- getFromNamespace("toggle_slider_server", "teal.goshawk")
#'
#' ui <- div(
#'   toggle_slider_ui(
#'     "toggle_slider", "Select value"
#'   ),
#'   verbatimTextOutput("value")
#' )
#'
#' server <- function(input, output, session) {
#'   is_dichotomous_slider <- (length(value) == 2)
#'   range_value <- toggle_slider_server("toggle_slider")
#'   messages <- reactiveVal() # to keep history
#'   observeEvent(range_value$state(), {
#'     list_with_names_str <- function(x) paste(names(x), x, sep = ": ", collapse = ", ")
#'     messages(c(messages(), list_with_names_str(range_value$state())))
#'   })
#'   output$value <- renderText({
#'     paste(messages(), collapse = "\n")
#'   })
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' @name toggle_sidebar
#' @keywords internal
#' @return `NULL`.
NULL


#' @rdname toggle_sidebar
toggle_slider_ui <- function(id, label) {
  ns <- NS(id)
  tags$div(
    tags$div(
      class = "flex justify-between mb-1",
      tags$span(tags$strong(label)),
      actionButton(ns("toggle"), "Toggle", class = "btn-xs")
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

#' @param ... additional parameters to pass to `sliderInput`
#' @param initial_state `reactiveValues` list with min, max and value.
#' @keywords internal
#' @rdname toggle_slider
toggle_slider_server <- function(id, initial_state, print = FALSE, ...) {
  moduleServer(id, function(input, output, session) {
    selected_state <- reactiveVal(NULL)
    slider_update_state <- reactiveVal(NULL)
    numeric_update_state <- reactiveVal(NULL)
    slider_shown <- reactive(input$toggle %% 2 == 0)
    observeEvent(initial_state$change_counter, {
      selected_state(
        list(
          min = initial_state$min,
          max = initial_state$max,
          value = initial_state$value
        )
      )
      slider_update_state(
        list(
          min = initial_state$min,
          max = initial_state$max,
          value = initial_state$value,
          change_counter = initial_state$change_counter
        )
      )
      numeric_update_state(
        list(
          min = initial_state$value[1],
          max = initial_state$value[2],
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
        step = NULL,
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
        numeric_update_state(list(min = input$slider[1], max = input$slider[2]))
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
    observeEvent(numeric_update_state(), {
      updateNumericInput(session, "value_low", value = numeric_update_state()$min)
      updateNumericInput(session, "value_high", value = numeric_update_state()$max)
    })

    return(selected_state)
  })
}

#' @keywords internal
#' @rdname toggle_slider
keep_slider_state_updated <- function(intial_state, varname, paramname, ANL, trt_group = NULL) { # nolint object_name_linter
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
    # step <- round(dmax / 100, 5) #TODO add step argument to the reactive list.
  }
  intial_state$min <- minmax[[1]]
  intial_state$max <- minmax[[2]]
  intial_state$value <- minmax
  intial_state$change_counter <- isolate(intial_state$change_counter) + 1
  intial_state
}
