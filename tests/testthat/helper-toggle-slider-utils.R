click_toggle_button <- function(app) {
  app$click(NS(app$active_ns()$module, "yrange_scale-toggle"))
}

#' Extract the values and the ranges from the UI for the slider
get_ui_slider_values <- function(app) {
  id <- NS(app$active_ns()$module, "yrange_scale-slider_view")
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

#' Checking if the sliderInput and the numericInputs match
check_if_widgets_match <- function(app) {
  testthat::expect_identical(
    app$get_active_module_input("yrange_scale-slider"),
    c(
      app$get_active_module_input("yrange_scale-value_low"),
      app$get_active_module_input("yrange_scale-value_high")
    )
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
  testthat::expect_identical(slider_values, values)
  testthat::expect_identical(
    app$get_active_module_input("yrange_scale-value_low"),
    as.integer(values$value[1])
  )
  testthat::expect_identical(
    app$get_active_module_input("yrange_scale-value_high"),
    as.integer(values$value[2])
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
  app$set_input(
    NS(app$active_ns()$module, "yrange_scale-slider"),
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
  app$set_input(
    NS(app$active_ns()$module, "yrange_scale-value_low"),
    values[1],
    wait_ = FALSE
  )
  app$set_input(
    NS(app$active_ns()$module, "yrange_scale-value_high"),
    values[2],
    wait_ = FALSE
  )
}
