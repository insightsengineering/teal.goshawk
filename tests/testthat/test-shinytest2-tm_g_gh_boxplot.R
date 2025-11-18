tm_g_gh_boxplot_driver <- function() {
  init_teal_app_driver(
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
      hline_vars_colors = c("pink", "brown", "purple", "black")
    )
  )
}

testthat::test_that("toggle_slider_module: widgets are initialized with proper values", {
  app_driver <- tm_g_gh_boxplot_driver()
  expect_setequal(
    app_driver$get_active_module_input("yrange_scale-slider"),
    c(0L, 55L)
  )
  app_driver$click(app_driver$namespaces(TRUE)$module("yrange_scale-toggle"))
  app_driver$wait_for_idle()
  expect_equal(app_driver$get_active_module_input("yrange_scale-value_low"), 0L)
  expect_equal(app_driver$get_active_module_input("yrange_scale-value_high"), 55L)
  app_driver$stop()
})

testthat::test_that("toggle_slider_module: changing the sliderInput sets proper numericInput values", {
  app_driver <- tm_g_gh_boxplot_driver()
  app_driver$set_active_module_input(
    "yrange_scale-slider",
    c(1L, 50L)
  )
  app_driver$click(app_driver$namespaces(TRUE)$module("yrange_scale-toggle"))
  app_driver$wait_for_idle()
  expect_equal(app_driver$get_active_module_input("yrange_scale-value_low"), 1L)
  expect_equal(app_driver$get_active_module_input("yrange_scale-value_high"), 50L)
  app_driver$stop()
})

testthat::test_that(
  "toggle_slider_module: changing the numericInputs
  within the sliderInput range, sets proper sliderInput values",
  {
    app_driver <- tm_g_gh_boxplot_driver()
    app_driver$click(app_driver$namespaces(TRUE)$module("yrange_scale-toggle"))
    app_driver$wait_for_idle()
    initial_range <- list(min = 0L, max = 55L)
    new_value <- c(10L, 40L)
    app_driver$set_active_module_input("yrange_scale-value_low", new_value[1])
    app_driver$wait_for_idle()
    app_driver$set_active_module_input("yrange_scale-value_high", new_value[2])
    app_driver$click(app_driver$namespaces(TRUE)$module("yrange_scale-toggle"))
    app_driver$wait_for_idle()
    expect_identical(
      list(
        min = app_driver$get_js(
          "document.querySelector('.teal-goshawk.toggle-slider-container input').getAttribute('data-min')"
        ) %>% as.integer(),
        max = app_driver$get_js(
          "document.querySelector('.teal-goshawk.toggle-slider-container input').getAttribute('data-max')"
        ) %>% as.integer(),
        value = app_driver$get_active_module_input("yrange_scale-slider")
      ),
      list(
        min = initial_range$min,
        max = initial_range$max,
        value = new_value
      )
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "toggle_slider_module: changing the numericInputs
  outside the sliderInput range, sets proper sliderInput values and range",
  {
    app_driver <- tm_g_gh_boxplot_driver()
    app_driver$click(app_driver$namespaces(TRUE)$module("yrange_scale-toggle"))
    app_driver$wait_for_idle()
    new_range <- c(-5L, 60L)
    app_driver$set_active_module_input("yrange_scale-value_low", new_range[1])
    app_driver$wait_for_idle()
    app_driver$set_active_module_input("yrange_scale-value_high", new_range[2])
    app_driver$click(app_driver$namespaces(TRUE)$module("yrange_scale-toggle"))
    app_driver$wait_for_idle()
    expect_identical(
      list(
        min = app_driver$get_js(
          "document.querySelector('.teal-goshawk.toggle-slider-container input').getAttribute('data-min')"
        ) %>% as.integer(),
        max = app_driver$get_js(
          "document.querySelector('.teal-goshawk.toggle-slider-container input').getAttribute('data-max')"
        ) %>% as.integer(),
        value = app_driver$get_active_module_input("yrange_scale-slider")
      ),
      list(
        min = new_range[1],
        max = new_range[2],
        value = new_range
      )
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "toggle_slider_module: changing the numericInputs
  within the rage, sets back the sliderInput range to initial range",
  {
    app_driver <- tm_g_gh_boxplot_driver()
    app_driver$click(app_driver$namespaces(TRUE)$module("yrange_scale-toggle"))
    app_driver$wait_for_idle()
    initial_range <- list(min = 0L, max = 55L)
    new_value <- c(11L, 30L)
    app_driver$set_active_module_input("yrange_scale-value_low", new_value[1])
    app_driver$wait_for_idle()
    app_driver$set_active_module_input("yrange_scale-value_high", new_value[2])
    app_driver$click(app_driver$namespaces(TRUE)$module("yrange_scale-toggle"))
    app_driver$wait_for_idle()
    expect_identical(
      list(
        min = app_driver$get_js(
          "document.querySelector('.teal-goshawk.toggle-slider-container input').getAttribute('data-min')"
        ) %>% as.integer(),
        max = app_driver$get_js(
          "document.querySelector('.teal-goshawk.toggle-slider-container input').getAttribute('data-max')"
        ) %>% as.integer(),
        value = app_driver$get_active_module_input("yrange_scale-slider")
      ),
      list(
        min = initial_range$min,
        max = initial_range$max,
        value = new_value
      )
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "toggle_slider_module: changing dependant widgets outside
sets proper sliderInput and numericInput values",
  {
    app_driver <- tm_g_gh_boxplot_driver()
    app_driver$set_active_module_input("xaxis_param", "CRP")
    new_range <- c(5L, 13L)
    expect_identical(
      list(
        min = app_driver$get_js(
          "document.querySelector('.teal-goshawk.toggle-slider-container input').getAttribute('data-min')"
        ) %>% as.integer(),
        max = app_driver$get_js(
          "document.querySelector('.teal-goshawk.toggle-slider-container input').getAttribute('data-max')"
        ) %>% as.integer(),
        value = app_driver$get_active_module_input("yrange_scale-slider")
      ),
      list(
        min = new_range[1],
        max = new_range[2],
        value = new_range
      )
    )
    app_driver$stop()
  }
)
