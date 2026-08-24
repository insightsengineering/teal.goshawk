local_log_threshold("WARN")

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

describe("tm_g_gh_density_distribution_plot: invalid arguments", {
  wrong_params <- list(
    label = 2L,
    dataname = list("A list of strings"),
    param = list("A list of strings"),
    xaxis_var = 1.5,
    trt_group = TRUE,
    color_manual = c(1, 2, 3, 4),
    comb_line = 123,
    font_size = c("12", "8", "20"),
    line_size = c("1", "0.25", "3"),
    hline_arb = c("0.02", "0.05"),
    comb_line = "TRUE",
    facet_ncol = "two",
    rotate_xlab = "FALSE",
    plot_height = c("600", "300", "2000"),
    plot_width = c("1000", "500", "2000"),
    pre_output = 123L,
    post_output = list(TRUE),
    transformators = list("not a function")
  )
  for (wrong_param in names(wrong_params)) {
    it(paste0("throws an error when ", wrong_param, " is invalid"), {
      args <- list(label = "module")
      args[[wrong_param]] <- wrong_params[[wrong_param]]
      expect_error(
        suppressWarnings(do.call(tm_g_gh_density_distribution_plot, args), classes = "pick_delayed"),
        sprintf("Assertion on '%s' failed", wrong_param)
      )
    })
  }

  it("throws error when hline_arb_label is invalid", {
    args <- list(label = "module", hline_arb = c(0.02, 0.05))
    args$hline_arb_label <- c(1, 2)
    expect_error(
      suppressWarnings(do.call(tm_g_gh_density_distribution_plot, args), classes = "pick_delayed"),
      "Assertion on 'hline_arb_label' failed"
    )
  })

  it("throws error when hline_arb_color is invalid", {
    args <- list(label = "module", hline_arb = c(0.02, 0.05), hline_arb_label = c("Line A", "Line B"))
    args$hline_arb_color <- c(1, 2)
    expect_error(
      suppressWarnings(do.call(tm_g_gh_density_distribution_plot, args), classes = "pick_delayed"),
      "Assertion on 'hline_arb_color' failed"
    )
  })
})
