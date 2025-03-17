# app_driver <- init_teal_app_driver(
#   data = get_test_data(),
#   modules = tm_g_gh_boxplot(
#     label = "Box Plot",
#     dataname = "ADLB",
#     param_var = "PARAMCD",
#     param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#     yaxis_var = choices_selected(c("AVAL", "BASE", "CHG"), "AVAL"),
#     xaxis_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "STUDYID"), "ARM"),
#     facet_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "SEX"), "AVISITCD"),
#     trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
#     loq_legend = TRUE,
#     rotate_xlab = FALSE,
#     hline_arb = c(60, 55),
#     hline_arb_color = c("grey", "red"),
#     hline_arb_label = c("default_hori_A", "default_hori_B"),
#     hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#     hline_vars_colors = c("pink", "brown", "purple", "black"),
#   )
# )

app_driver <- function() {
  driver <- init_teal_app_driver(
    data = get_test_data(),
    modules = tm_g_gh_boxplot(
      label = "Box Plot",
      dataname = "ADLB",
      param_var = "PARAMCD",
      param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
      yaxis_var = choices_selected(c("AVAL", "BASE", "CHG"), "AVAL"),
      xaxis_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "STUDYID"), "ARM"),
      facet_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "SEX"), "AVISITCD"),
      trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
      loq_legend = TRUE,
      rotate_xlab = FALSE,
      hline_arb = c(60, 55),
      hline_arb_color = c("grey", "red"),
      hline_arb_label = c("default_hori_A", "default_hori_B"),
      hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
      hline_vars_colors = c("pink", "brown", "purple", "black"),
    )
  )
  driver
}

testthat::test_that("toggle_slider_module: widgets are initialized with proper values", {
  app_driver <- app_driver()
  app_driver$wait_for_idle()
  init_values <- list(min = 0, max = 55, value = c(0, 55))
  check_widgets_with_value(app_driver, init_values)
})

testthat::test_that("toggle_slider_module: changing the sliderInput sets proper numericInput values", {
  app_driver <- app_driver()
  app_driver$wait_for_idle()
  set_slider_values(app_driver, c(1, 50))
  check_widgets_with_value(
    app_driver,
    list(min = 0, max = 55, value = c(1, 50))
  )
})

testthat::test_that(
  "toggle_slider_module: changing the numericInputs
  within the sliderInput range, sets proper sliderInput values",
  {
    app_driver <- app_driver()
    app_driver$wait_for_idle()
    initial_range <- list(min = 0, max = 55)
    new_value <- c(10, 40)
    set_numeric_input_low(app_driver, new_value[1])
    set_numeric_input_high(app_driver, new_value[2])
    check_widgets_with_value(
      app_driver,
      list(
        min = initial_range$min,
        max = initial_range$max,
        value = new_value
      )
    )
  }
)

testthat::test_that(
  "toggle_slider_module: changing the numericInputs
  outside the sliderInput range, sets proper sliderInput values and range",
  {
    app_driver <- app_driver()
    app_driver$wait_for_idle()
    new_range <- c(-5, 60)
    set_numeric_input_low(app_driver, new_range[1])
    set_numeric_input_high(app_driver, new_range[2])
    check_widgets_with_value(
      app_driver,
      list(
        min = new_range[1],
        max = new_range[2],
        value = c(new_range[1], new_range[2])
      )
    )
  }
)

testthat::test_that(
  "toggle_slider_module: changing the numericInputs
  within the rage, sets back the sliderInput range to initial range",
  {
    app_driver <- app_driver()
    app_driver$wait_for_idle()
    initial_range <- list(min = 0, max = 55)
    new_value <- c(11, 30)
    set_numeric_input_low(app_driver, new_value[1])
    set_numeric_input_high(app_driver, new_value[2])
    check_widgets_with_value(
      app_driver,
      list(
        min = initial_range$min,
        max = initial_range$max,
        value = c(new_value[1], new_value[2])
      )
    )
  }
)

testthat::test_that(
  "toggle_slider_module: changing dependant widgets outside
sets proper sliderInput and numericInput values",
  {
    app_driver <- app_driver()
    app_driver$wait_for_idle()
    app_driver$set_active_module_input("xaxis_param", "CRP")
    new_range <- c(5, 13)
    check_widgets_with_value(
      app_driver,
      list(
        min = new_range[1],
        max = new_range[2],
        value = c(new_range[1], new_range[2])
      )
    )
    app_driver$stop()
  }
)
