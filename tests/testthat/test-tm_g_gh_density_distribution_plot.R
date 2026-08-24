threshold <- logger::log_threshold(namespace = "teal.goshawk")
logger::log_threshold("ERROR", namespace = "teal.goshawk")
withr::defer(logger::log_threshold(threshold, namespace = "teal.goshawk"))

test_that("tm_g_gh_density_distribution_plot can be created with default arguments", {
  module <- suppressWarnings(
    tm_g_gh_density_distribution_plot(label = "Density Distribution Plot", dataname = "ADLB"),
    classes = "pick_delayed"
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Density Distribution Plot")
})

test_that("tm_g_gh_density_distribution_plot can be created with choices_selected arguments", {
  withr::local_options(lifecycle_verbosity = "quiet")
  module <- tm_g_gh_density_distribution_plot(
    label = "Density Distribution Plot",
    dataname = "ADLB",
    param_var = "PARAMCD",
    param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
    xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
    trt_group = choices_selected(c("ARM", "ACTARM"), "ARM")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Density Distribution Plot")
})

test_that("tm_g_gh_density_distribution_plot can be created with custom picks arguments", {
  module <- tm_g_gh_density_distribution_plot(
    label = "Density Distribution Plot",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Density Distribution Plot")
})

test_that("tm_g_gh_density_distribution_plot handles different parameter combinations", {
  module <- tm_g_gh_density_distribution_plot(
    label = "Density Distribution Plot - Complex",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "CRP", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ACTARM"),
    facet_ncol = 2,
    comb_line = TRUE,
    rotate_xlab = FALSE
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Density Distribution Plot - Complex")
})

test_that("tm_g_gh_density_distribution_plot accepts custom color specifications", {
  module <- tm_g_gh_density_distribution_plot(
    label = "Density Distribution Plot - Styled",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    color_manual = c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C"),
    color_comb = "#39ff14",
    font_size = c(12, 8, 20),
    line_size = c(1, 0.25, 3)
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Density Distribution Plot - Styled")
})

test_that("tm_g_gh_density_distribution_plot accepts line arguments", {
  module <- tm_g_gh_density_distribution_plot(
    label = "Density Distribution Plot - With Lines",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    hline_arb = c(0.02, 0.05),
    hline_arb_label = c("Line A", "Line B"),
    hline_arb_color = c("red", "black"),
    comb_line = TRUE
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Density Distribution Plot - With Lines")
})

test_that("tm_g_gh_density_distribution_plot handles plot dimensions correctly", {
  module <- tm_g_gh_density_distribution_plot(
    label = "Density Distribution Plot - Sized",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    plot_height = c(600, 300, 2000),
    plot_width = c(1000, 500, 2000)
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Density Distribution Plot - Sized")
})
