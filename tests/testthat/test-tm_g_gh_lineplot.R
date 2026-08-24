local_log_threshold("WARN")

test_that("tm_g_gh_lineplot can be created with default arguments", {
  module <- suppressWarnings(
    tm_g_gh_lineplot(label = "Line Plot", dataname = "ADLB"),
    classes = "pick_delayed"
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Line Plot")
})

test_that("tm_g_gh_lineplot can be created with choices_selected arguments", {
  withr::local_options(lifecycle_verbosity = "quiet")
  module <- tm_g_gh_lineplot(
    label = "Line Plot",
    dataname = "ADLB",
    param_var = "PARAMCD",
    param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
    xaxis_var = choices_selected(c("AVISITCD", "AVISIT"), "AVISITCD"),
    yaxis_var = choices_selected(c("AVAL", "CHG", "PCHG"), "AVAL"),
    trt_group = choices_selected(c("ARM", "ACTARM"), "ARM")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Line Plot")
})

test_that("tm_g_gh_lineplot can be created with custom picks arguments", {
  module <- tm_g_gh_lineplot(
    label = "Line Plot",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVISITCD", "AVISIT"), "AVISITCD"),
    yaxis_var = teal.picks::variables(c("AVAL", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Line Plot")
})

test_that("tm_g_gh_lineplot handles multiple parameter combinations", {
  module <- tm_g_gh_lineplot(
    label = "Line Plot - Complex",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "CRP", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVISITCD", "AVISIT"), "AVISIT"),
    yaxis_var = teal.picks::variables(c("AVAL", "CHG", "PCHG"), "CHG"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ACTARM"),
    stat = "median",
    plot_relative_height_value = 1500
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Line Plot - Complex")
})

test_that("tm_g_gh_lineplot accepts custom color specifications with picks", {
  module <- tm_g_gh_lineplot(
    label = "Line Plot - Styled",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVISITCD", "AVISIT"), "AVISITCD"),
    yaxis_var = teal.picks::variables(c("AVAL", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    color_manual = c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C"),
    plot_font_size = c(12, 8, 20),
    dot_size = c(2, 1, 12),
    dodge = c(0.4, 0, 1)
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Line Plot - Styled")
})

test_that("tm_g_gh_lineplot accepts line arguments with picks", {
  module <- tm_g_gh_lineplot(
    label = "Line Plot - With Lines",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVISITCD", "AVISIT"), "AVISITCD"),
    yaxis_var = teal.picks::variables(c("AVAL", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    hline_arb = c(40, 50),
    hline_arb_label = c("line_1", "line_2"),
    hline_arb_color = c("red", "blue")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Line Plot - With Lines")
})
