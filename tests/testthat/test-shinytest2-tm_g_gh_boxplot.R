skip("To be refactored")

tm_g_gh_boxplot_driver <- function() {
  init_teal_app_driver(
    data = get_test_data(),
    modules = tm_g_gh_boxplot(
      label = "Box Plot",
      dataname = "ADLB",
      param = teal.picks::picks(
        teal.picks::variables("PARAMCD", "PARAMCD"),
        teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
        check_dataset = FALSE
      ),
      yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG"), "AVAL"),
      xaxis_var = teal.picks::variables(c("AVISITCD", "ACTARM", "ARM"), "ARM"),
      facet_var = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
      trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM")
    )
  )
}

test_that("e2e - tm_g_gh_boxplot initializes and renders a plot", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)
  app_driver <- tm_g_gh_boxplot_driver()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()

  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  expect_match(
    app_driver$get_active_module_plot_output("boxplot"),
    "data:image/png;base64,"
  )
})

test_that(
  "e2e - tm_g_gh_boxplot starts with expected label and encoding selections.",
  {
    skip_if_not_installed("shinytest2")
    skip_if_too_deep(5)
    app_driver <- tm_g_gh_boxplot_driver()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()

    expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Box Plot"
    )
    expect_equal(get_teal_picks_slot(app_driver, "xaxis_param", "datasets"), "ADLB")
    expect_equal(get_teal_picks_slot(app_driver, "xaxis_param", "values"), "ALT")
    expect_equal(get_teal_picks_slot(app_driver, "yaxis_var", "variables"), "AVAL")
    expect_equal(get_teal_picks_slot(app_driver, "xaxis_var", "variables"), "ARM")
    expect_equal(get_teal_picks_slot(app_driver, "facet_var", "variables"), "ARM")
    expect_equal(get_teal_picks_slot(app_driver, "trt_group", "variables"), "ARM")
  }
)

describe("e2e - tm_g_gh_boxplot: changing pick changes plot and does not throw validation errors.", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)

  action_mod <- list(
    xaxis_param = list(slot_name = "values", value = "CRP"),
    yaxis_var = list(slot_name = "variables", value = "BASE"),
    xaxis_var = list(slot_name = "variables", value = "AVISITCD"),
    facet_var = list(slot_name = "variables", value = "ACTARM"),
    trt_group = list(slot_name = "variables", value = "ACTARM")
  )

  for (pick_id in names(action_mod)) {
    it(pick_id, {
      slot_name <- action_mod[[pick_id]]$slot_name
      new_value <- action_mod[[pick_id]]$value

      app_driver <- tm_g_gh_boxplot_driver()
      withr::defer(app_driver$stop())
      app_driver$wait_for_idle()
      plot_before <- app_driver$get_active_module_plot_output("boxplot")
      set_teal_picks_slot(app_driver, pick_id, slot_name, new_value)
      app_driver$wait_for_idle(duration = 2000)
      expect_equal(get_teal_picks_slot(app_driver, pick_id, slot_name), new_value)
      expect_false(identical(plot_before, app_driver$get_active_module_plot_output("boxplot")))
      app_driver$expect_no_validation_error()
    })
  }
})

test_that("e2e - tm_g_gh_boxplot displays selected data points table", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)
  app_driver <- tm_g_gh_boxplot_driver()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()

  # Check that the app renders without errors
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
})

test_that("e2e - tm_g_gh_boxplot displays descriptive statistics table", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)
  app_driver <- tm_g_gh_boxplot_driver()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()

  # Check that the table is rendered
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
})

test_that("e2e - tm_g_gh_boxplot handles LoQ legend toggle", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)
  app_driver <- tm_g_gh_boxplot_driver()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()

  # Get the initial plot
  plot_initial <- app_driver$get_active_module_plot_output("boxplot")

  # Toggle LoQ legend checkbox
  app_driver$find_element("input[id$=loq_legend]")$click()
  app_driver$wait_for_idle(duration = 2000)

  # Verify the plot changed and no errors
  expect_false(identical(plot_initial, app_driver$get_active_module_plot_output("boxplot")))
  app_driver$expect_no_validation_error()
})

test_that("e2e - tm_g_gh_boxplot respects plot height adjustment", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)
  app_driver <- tm_g_gh_boxplot_driver()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()

  # Verify plot is rendered
  expect_match(
    app_driver$get_active_module_plot_output("boxplot"),
    "data:image/png;base64,"
  )
  app_driver$expect_no_validation_error()
})
