click_toggle_button <- function(app) {
  app$click(NS(app$active_ns()$module, "yrange_scale-toggle"))
}

#' Extract the values and the ranges from the UI for the slider
get_ui_slider_values <- function(app) {
  id <- NS(app$active_ns()$module, "yrange_scale-inputs")
  # Note that the values can only be observed once they are visible
  if (!is_slider_visible(app)) {
    click_toggle_button(app)
  }
  list(
    min = app$get_text(sprintf("#%s .irs-min", id)) |> as.numeric(),
    max = app$get_text(sprintf("#%s .irs-max", id)) |> as.numeric(),
    value = c(
      app$get_text(sprintf("#%s .irs-from", id)),
      app$get_text(sprintf("#%s .irs-to", id))
    ) |> as.numeric()
  )
}

#' Extract the values and the ranges from the numeric widgets
get_numeric_values <- function(app) {
  id <- NS(app$active_ns()$module, "yrange_scale-inputs")
  # Note that the values can only be observed once they are visible
  if (is_slider_visible(app)) {
    click_toggle_button(app)
  }
  c(
    app$get_active_module_input("yrange_scale-value_low"),
    app$get_active_module_input("yrange_scale-value_high")
  )
}

#' Checking if the sliderInput and the numericInputs with custom values.
#' values must be a list with min, max, value as keys.
#' check_widgets_with_value(app, list(min = 0, max = 55, value = c(0, 55)))
check_widgets_with_value <- function(app, values) {
  checkmate::assert_list(values, types = "numeric", min.len = 3)
  checkmate::assert_names(names(values), must.include = c("min", "max", "value"))
  checkmate::assert_numeric(values$value, len = 2)
  slider_values <- get_ui_slider_values(app)
  numeric_values <- get_numeric_values(app)
  testthat::expect_identical(slider_values, values)
  testthat::expect_setequal(
    numeric_values,
    values$value
  )
}

is_slider_visible <- function(app) {
  app$get_active_module_input("yrange_scale-toggle") %% 2 == 0
}

#' values should be a numeric vector of length 2
#' Note that it will automatically toggle slider to be visible before setting it
set_slider_values <- function(app, values) {
  checkmate::assert_numeric(values, len = 2)

  if (!is_slider_visible(app)) {
    click_toggle_button(app)
  }
  app$set_active_module_input(
    "yrange_scale-slider",
    values,
    wait_ = FALSE
  )
}

#' values should be a numeric vector of length 2
#' Note that it will automatically toggle slider to be visible before setting it
set_numeric_input_values <- function(app, values) {
  checkmate::assert_numeric(values, len = 2)

  if (is_slider_visible(app)) {
    click_toggle_button(app)
  }
  app$set_active_module_input(
    "yrange_scale-value_low",
    values[1],
    wait_ = FALSE
  )
  app$set_active_module_input(
    "yrange_scale-value_high",
    values[2],
    wait_ = FALSE
  )
}
