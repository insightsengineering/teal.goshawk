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
#' @param min `numeric or integer` minimum value
#' @param max `numeric or integer` maximum value
#' @param value `numeric or integer` either of length 1 for normal slider or of
#'   length 2 for dichotomous slider.
#' @param slider_initially `logical` whether to show slider or numeric fields
#'   initially
#' @param step_numeric `numeric or integer` step for numeric input fields
#' @param width `numeric` width of slider or of each numeric field
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
#'     "toggle_slider", "Select value",
#'     min = 0.2, max = 100.1, value = value,
#'     slider_initially = FALSE, step_numeric = 0.001
#'   ),
#'   verbatimTextOutput("value")
#' )
#'
#' server <- function(input, output, session) {
#'   is_dichotomous_slider <- (length(value) == 2)
#'   range_value <- toggle_slider_server("toggle_slider",
#'     is_dichotomous_slider = is_dichotomous_slider,
#'     step_slider = 0.1
#'   )
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
#' @rdname toggle_sidebar
#' @keywords internal
#' @return `NULL`.
NULL


#' @rdname toggle_sidebar
toggle_slider_ui <- function(id,
                             label,
                             min,
                             max,
                             value,
                             slider_initially = TRUE,
                             step_slider = NULL,
                             step_numeric = step_slider,
                             width = NULL,
                             ...) {
  checkmate::assert_number(min)
  checkmate::assert_number(max)
  checkmate::assert_flag(slider_initially)
  checkmate::assert_number(step_slider, null.ok = TRUE)
  checkmate::assert_number(step_numeric, null.ok = TRUE)
  checkmate::assert_numeric(value, min.len = 1, max.len = 2)
  if (is.null(step_numeric)) {
    step_numeric <- NA # numericInput does not support NULL
  }

  show_or_not <- function(show) if (show) identity else shinyjs::hidden
  ns <- NS(id)
  tags$div(
    include_css_files("custom"),
    shinyjs::useShinyjs(),
    tags$div(
      class = "flex justify-between mb-1",
      tags$span(tags$strong(label)),
      actionButton(ns("toggle"), "Toggle", class = "btn-xs")
    ),
    show_or_not(slider_initially)(
      uiOutput(ns("slider_ui"))
    ),
    show_or_not(!slider_initially)(tags$span(
      id = ns("numeric_view"),
      if (length(value) == 1) {
        numericInput(
          ns("value"),
          label = NULL,
          min = min,
          max = max,
          value = value[[1]],
          step = step_numeric,
          width = width
        )
      } else {
        tags$div(
          numericInput(
            ns("value_low"),
            "From:",
            min = min,
            max = max,
            value = value[[1]],
            step = step_numeric,
            width = width
          ),
          numericInput(
            ns("value_high"),
            "- to:",
            min = min,
            max = max,
            value = value[[2]],
            step = step_numeric,
            width = width
          )
        )
      }
    ))
  )
}

#' @param is_dichotomous_slider `logical` whether it is a dichotomous slider or normal slider
#' @param step_slider `numeric or integer` step for slider
#' @param ... additional parameters to pass to `sliderInput`
#' @keywords internal
#' @rdname toggle_slider
toggle_slider_server <- function(id, is_dichotomous_slider = TRUE, step_slider = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    checkmate::assert_flag(is_dichotomous_slider)
    # model view controller: cur_state is the model, the sliderInput and numericInputs are two views/controllers
    # additionally, the module returns the cur_state, so it can be controlled from that end as well
    cur_state <- reactiveVal(NULL) # model, can contain min, max, value etc.
    slider_range <- reactiveVal(NULL)


    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$condition(~ input$toggle %% 2 == 1)
      iv$add_rule("value_low", shinyvalidate::sv_required("A 'from' value is required - a default is used instead"))
      iv$add_rule("value_high", shinyvalidate::sv_required("A 'to' value is required - a default is used instead)"))
      iv$add_rule(
        "value_high",
        ~ if (!is.na(input$value_low) && (.) < input$value_low) {
          "'From' value should be lower than 'to' value - axis has been flipped"
        }
      )
      iv$add_rule(
        "value_low",
        ~ if (!is.na(input$value_high) && (.) > input$value_high) {
          "'To' value should be greater than 'from' value - axis has been flipped"
        }
      )
      iv$enable()
      iv
    })

    set_state <- function(new_state) {
      stopifnot(all(names(new_state) %in% c("min", "max", "step", "value")))
      iv_r()$is_valid()
      # when value does not fall into min, max range, it will automatically get truncated

      # only update provided components, do not discasrd others
      old_state <- cur_state()
      if (!is.null(old_state)) {
        new_state <- utils::modifyList(old_state, new_state)
      }

      if (!setequal(new_state, cur_state())) {
        cur_state(new_state)
      }
    }
    observeEvent(input$slider, {
      set_state(list(value = input$slider))
    })
    # two values for range (dichotomous slider)
    observeEvent(
      eventExpr = { # nolint
        input$value_low
        input$value_high
      },
      handlerExpr = { # nolint
        set_state(list(value = c(input$value_low, input$value_high)))
      }
    )
    # one value for value in range
    observeEvent(
      input$value,
      handlerExpr = { # nolint
        set_state(list(value = input$value))
      }
    )

    slider_states <- reactive({
      state_slider <- cur_state()
      req(length(state_slider) > 0) # update will otherwise not work
      state_low <- state_slider
      state_high <- state_slider
      if (length(state_slider$value) > 1) {
        state_low$value <- state_low$value[[1]]
        state_high$value <- state_high$value[[2]]
      }
      state_slider$max <- max(state_slider$max, state_slider$value[2])
      state_slider$min <- min(state_slider$min, state_slider$value[1])
      list(
        low = state_low,
        high = state_high,
        low_value = state_low$value,
        high_value = state_high$value,
        slider_value = state_slider$value,
        slider_max = state_slider$max,
        slider_min = state_slider$min
      )
    })

    update_widgets <- function() {
      state <- slider_states()
      if (input$toggle %% 2 != 0) {
        if (length(state$slider_value) > 1) {
          do.call(updateNumericInput, c(list(session, "value_low"), state$low))
          do.call(updateNumericInput, c(list(session, "value_high"), state$high))
        } else {
          do.call(updateNumericInput, c(list(session, "value"), state$low))
        }
      }
    }
    observeEvent(input$toggle, {
      update_widgets()
      shinyjs::toggle("numeric_view")
      shinyjs::toggle("slider_ui")
    })


    output$slider_ui <- renderUI({
      req(input$toggle >= 0)
      req(slider_range())
      state <- isolate(slider_states())
      args <- list(
        inputId = session$ns("slider"),
        label = NULL,
        min = state$slider_min,
        max = state$slider_max,
        value = state$slider_value,
        step = step_slider,
        ...
      )
      if (length(seq(state$slider_min, state$slider_max)) < 10) {
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

    update_toggle_slider <- function(value = NULL, min = NULL, max = NULL, step = NULL) {
      if (!is.null(value) && is_dichotomous_slider) {
        stopifnot(length(value) == 2)
      }
      set_state(Filter(Negate(is.null), list(value = value, min = min, max = max, step = step)))
      slider_range(list(value = value, min = min, max = max, step = step))
      update_widgets()
    }
    return(list(
      state = cur_state,
      update_state = update_toggle_slider
    ))
  })
}
