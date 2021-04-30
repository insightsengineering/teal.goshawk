#' UI with a toggable slider to change between slider and numeric input fields
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
#' @param step_slider `numeric or integer` step for slider
#' @param step_numeric `numeric or integer` step for numeric input fields
#' @param width `numeric` width of slider or of each numeric field
#' @param ... additional parameters to pass to `sliderInput`
#'
#' @return Shiny HTML UI
#'
#' @examples
#' value <- c(20.3, 81.5) # dichotomous slider
#' # value <- c(50.1) # normal slider
#' app <- shinyApp(
#'   ui = div(
#'     teal.goshawk:::toggle_slider_ui(
#'       "toggle_slider", "Select value", min = 0.2, max = 100.1, value = value,
#'       slider_initially = FALSE, step_slider = 0.1, step_numeric = 0.001
#'     ),
#'     verbatimTextOutput("value")
#'   ),
#'   server = function(input, output, session) {
#'     is_dichotomous_slider <- (length(value) == 2)
#'     range_value <- callModule(teal.goshawk:::toggle_slider_server, "toggle_slider",
#'       is_dichotomous_slider = is_dichotomous_slider)
#'     messages <- reactiveVal() # to keep history
#'     observeEvent(range_value$state(), {
#'       list_with_names_str <- function(x) paste(names(x), x, sep = ": ", collapse = ", ")
#'       messages(c(messages(), list_with_names_str(range_value$state())))
#'     })
#'     output$value <- renderText({
#'       paste(messages(), collapse = "\n")
#'     })
#'
#'     # for stress-testing example, update slider settings
#'     # bug with invalidateLater not working inside `observeEvent`
#'     # observe({
#'     #   invalidateLater(1000, session)
#'     #   a <- sample(0:100, 1) # for range
#'     #   b <- sample(0:100, 1)
#'     #   isolate(do.call(
#'     #     range_value$update_state,
#'     #     list(
#'     #       value = sort(sample(0:100, if (is_dichotomous_slider) 2 else 1)),
#'     #       min = min(a, b), max = max(a, b),
#'     #       step = sample(1:20, 1) / 10
#'     #     )[sample(1:4, sample(4, 1))] # select up to four fields from the list
#'     #   ))
#'     # })
#'   }
#' )
#' shinyApp(app$ui, app$server) %>% invisible()
toggle_slider_ui <- function(id,
                             label,
                             min,
                             max,
                             value,
                             slider_initially = TRUE,
                             step_slider = NULL,
                             step_numeric = step_slider,
                             width = NULL, ...) {
  is_numeric_like <- function(x) is_numeric_single(x) || is_integer_single(x)

  stopifnot(
    is_numeric_like(min),
    is_numeric_like(max),
    is_logical_single(slider_initially),
    is.null(step_slider) || is_numeric_like(step_slider),
    is.null(step_numeric) || is_numeric_like(step_numeric),
    is_numeric_vector(value) || is_integer_vector(value), length(value) %in% c(1, 2)
  )
  if (is.null(step_numeric)) {
    step_numeric <- NA # numericInput does not support NULL
  }

  show_or_not <- function(show) if (show) identity else shinyjs::hidden
  ns <- NS(id)
  div(
    shinyjs::useShinyjs(),
    actionButton(ns("toggle"), "Toggle"),
    show_or_not(slider_initially)(
      sliderInput(
        ns("slider"),
        label = label,
        min = min,
        max = max,
        value = value,
        step = step_slider,
        width = width,
        ...)
    ),
    show_or_not(!slider_initially)(tags$span(
      id = ns("numeric_view"),
      if (length(value) == 1) {
        numericInput(
          ns("value"),
          label = label,
          min = min,
          max = max,
          value = value[[1]],
          step = step_numeric,
          width = width)
      } else {
        div(
          tags$label(label),
          numericInput(
            ns("value_low"),
            "From:",
            min = min,
            max = max,
            value = value[[1]],
            step = step_numeric,
            width = width),
          numericInput(
            ns("value_high"),
            "- to:",
            min = min,
            max = max,
            value = value[[2]],
            step = step_numeric,
            width = width)
        )
      }
    ))
  )
}

# is_dichotomous_slider `logical` whether it is a dichotomous slider or normal slider
toggle_slider_server <- function(input, output, session, is_dichotomous_slider = TRUE, global_input = NULL) {
  stopifnot(is_logical_single(is_dichotomous_slider))
  # model view controller: cur_state is the model, the sliderInput and numericInputs are two views/controllers
  # additionally, the module returns the cur_state, so it can be controlled from that end as well
  cur_state <- reactiveVal(NULL) # model, can contain min, max, value etc.

  set_state <- function(new_state) {
    stopifnot(all(names(new_state) %in% c("min", "max", "step", "value")))
    if (all(c("min", "max") %in% names(new_state))) {
      validate(need(
        (new_state$min <= new_state$max),
        "Minimum value needs to be smaller than or equal to maximum value."
      ))
    }
    if (!is.null(new_state$value) && (length(new_state$value) == 2)) {
      validate(need(
        new_state$value[[1]] <= new_state$value[[2]],
        "Minimum value needs to be smaller than or equal to maximum value."
      ))
    }
    # when value does not fall into min, max range, it will automatically get truncated

    # only update provided components, do not discasrd others
    old_state <- cur_state()
    new_state <- c(new_state, old_state[!names(old_state) %in% names(new_state)])
    new_state <- new_state[sort(names(new_state))]
    cur_state(new_state)
  }
  observeEvent(input$slider, {
    set_state(list(value = input$slider))
  })
  # two values for range (dichotomous slider)
  observeEvent({
    input$value_low; input$value_high}, {
    set_state(list(value = c(input$value_low, input$value_high)))
  })
  # one value for value in range
  observeEvent(
    input$value, {
    set_state(list(value = input$value))
  })


  update_widgets <- function() {
    state_slider <- cur_state()
    req(length(state_slider) > 0) # update will otherwise not work
    state_low <- state_slider
    state_high <- state_slider
    if (!is.null(state_slider$value) && (length(state_slider$value) > 1)) {
      state_low$value <- state_low$value[[1]]
      state_high$value <- state_high$value[[2]]
    }
    if (input$toggle %% 2 == 0) {
      do.call(updateSliderInput, c(list(session, "slider"), state_slider))
    } else {
      if (length(state_slider$value) > 1) {
        do.call(updateNumericInput, c(list(session, "value_low"), state_low))
        do.call(updateNumericInput, c(list(session, "value_high"), state_high))
      } else {
        do.call(updateNumericInput, c(list(session, "value"), state_low))
      }
    }
  }
  observeEvent(cur_state(), handlerExpr = update_widgets(), once = TRUE)
  if (!is.null(global_input)) {
    observeEvent(list(global_input$xaxis_var, global_input$yaxis_var), update_widgets(), priority = -Inf)
  }
  observeEvent(input$toggle, {
    update_widgets()
    shinyjs::toggle("numeric_view")
    shinyjs::toggle("slider")
  })

  update_toggle_slider <- function(value = NULL, min = NULL, max = NULL, step = NULL) {
    if (!is.null(value) && is_dichotomous_slider) {
      stopifnot(length(value) == 2)
    }
    set_state(Filter(Negate(is.null), list(value = value, min = min, max = max, step = step)))
  }
  return(list(
    state = cur_state,
    update_state = update_toggle_slider
  ))
}