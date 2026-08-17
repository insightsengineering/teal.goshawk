tm_g_gh_boxplot_driver <- function() {
  init_teal_app_driver(
    data = get_test_data(),
    modules = tm_g_gh_correlationplot(
      label = "Correlation Plot",
      dataname = "ADLB",
      param_var = "PARAMCD",
      xaxis_param = teal.picks::values(c("ALT", "CRP", "IGA"), "ALT"),
      yaxis_param = teal.picks::values(c("ALT", "CRP", "IGA"), "CRP"),
      xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
      yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM")
    )
  )
}

test_that("e2e - tm_g_gh_correlationplot initializes and renders a plot", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)
  app_driver <- tm_g_gh_boxplot_driver()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()

  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  expect_match(
    app_driver$get_active_module_plot_output("plot"),
    "data:image/png;base64,"
  )
  expect_equal(
    nrow(app_driver$get_active_module_table_output("brush_data")),
    0
  )
})

test_that(
  "e2e - tm_g_gh_correlationplot starts with expected label and encoding selections.",
  {
    skip_if_not_installed("shinytest2")
    skip_if_too_deep(5)
    app_driver <- tm_g_gh_boxplot_driver()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()

    expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Correlation Plot"
    )
    expect_equal(get_teal_picks_slot(app_driver, "xaxis_param", "datasets"), "ADLB")
    expect_equal(get_teal_picks_slot(app_driver, "yaxis_param", "datasets"), "ADLB")
    expect_equal(get_teal_picks_slot(app_driver, "xaxis_param", "values"), "ALT")
    expect_equal(get_teal_picks_slot(app_driver, "yaxis_param", "values"), "CRP")
    expect_equal(get_teal_picks_slot(app_driver, "xaxis_var", "variables"), "BASE")
    expect_equal(get_teal_picks_slot(app_driver, "yaxis_var", "variables"), "AVAL")
    expect_equal(get_teal_picks_slot(app_driver, "trt_group", "variables"), "ARM")
  }
)

describe("e2e - tm_g_gh_correlationplot: changing pick changes plot and does not throw validation errors.", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)

  action_mod <- list(
    xaxis_param = list(slot_name = "values", value = "CRP"),
    yaxis_param = list(slot_name = "values", value = "ALT"),
    xaxis_var = list(slot_name = "variables", value = "CHG"),
    yaxis_var = list(slot_name = "variables", value = "BASE"),
    trt_group = list(slot_name = "variables", value = "ACTARM")
  )

  for (pick_id in names(action_mod)) {
    it(pick_id, {
      slot_name <- action_mod[[pick_id]]$slot_name
      new_value <- action_mod[[pick_id]]$value

      app_driver <- tm_g_gh_boxplot_driver()
      withr::defer(app_driver$stop())
      app_driver$wait_for_idle()
      plot_before <- app_driver$get_active_module_plot_output("plot")
      set_teal_picks_slot(app_driver, pick_id, slot_name, new_value)
      app_driver$wait_for_idle(duration = 2000)
      expect_equal(get_teal_picks_slot(app_driver, pick_id, slot_name), new_value)
      expect_false(identical(plot_before, app_driver$get_active_module_plot_output("plot")))
      app_driver$expect_no_validation_error()
    })
  }
})
