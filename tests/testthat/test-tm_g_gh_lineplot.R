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

describe("tm_g_gh_lineplot: invalid arguments", {
  wrong_params <- list(
    label = 2L,
    dataname = list("A list of strings"),
    param = list("A list of strings"),
    param_var_label = 123,
    xaxis_var = 1.5,
    yaxis_var = "1.5",
    trt_group = TRUE,
    stat = 42,
    color_manual = c(1, 2, 3, 4),
    rotate_xlab = data.frame(letters = letters),
    hline_arb = c("0.02", "0.05"),
    plot_height = c("600", "300", "2000"),
    plot_width = c("1000", "500", "2000"),
    plot_font_size = c("12", "8", "20"),
    dodge = c("0.4", "0", "1"),
    count_threshold = "0",
    table_font_size = c("12", "4", "20"),
    dot_size = c("2", "1", "12"),
    plot_relative_height_value = "1000",
    pre_output = 123L,
    post_output = list(TRUE),
    transformators = list("not a function"),
    decorators = 1L,
    xvar_level = 1L,
    trt_group_level = 1L,
    shape_choices = 1L,
    xtick = base::identity,
    xlabel = 1L
  )
  for (wrong_param in names(wrong_params)) {
    it(paste0("throws an error when ", wrong_param, " is invalid"), {
      args <- list(label = "module")
      args[[wrong_param]] <- wrong_params[[wrong_param]]
      expect_error(
        suppressWarnings(do.call(tm_g_gh_lineplot, args), classes = "pick_delayed"),
        sprintf("Assertion on '%s' failed", wrong_param)
      )
    })
  }

  wrong_params2 <- list(
    hline_arb_label = c(1, 2), hline_arb_color = c(1, 2)
  )
  for (wrong_param2 in names(wrong_params2)) {
    it(paste0("throws an error when ", wrong_param2, " is invalid"), {
      args <- list(
        label = "module",
        hline_arb = c(0.02, 0.05)
      )
      args[[wrong_param2]] <- wrong_params2[[wrong_param2]]
      expect_error(
        suppressWarnings(do.call(tm_g_gh_lineplot, args), classes = "pick_delayed"),
        sprintf("Assertion on '%s' failed", wrong_param2)
      )
    })
  }

  deprecated <- c("param_var", "filter_var", "filter_var_choices")
  for (deprecated_param in deprecated) {
    it(paste0("gives a deprecation warning when ", deprecated_param, " is used"), {
      args <- list(label = "module")
      args[[deprecated_param]] <- "PARAMCD"
      lifecycle::expect_deprecated(
        suppressWarnings(do.call(tm_g_gh_lineplot, args), classes = "pick_delayed")
      )
    })
  }

  it("all arguments are tested", {
    expect_setequal(unique(c(names(wrong_params), names(wrong_params2), deprecated)), names(formals(tm_g_gh_lineplot)))
  })
})
