local_log_threshold("WARN")

threshold <- logger::log_threshold(namespace = "teal.goshawk")
logger::log_threshold("ERROR", namespace = "teal.goshawk")
withr::defer(logger::log_threshold(threshold, namespace = "teal.goshawk"))

test_that("tm_g_gh_boxplot can be created with default arguments", {
  module <- suppressWarnings(
    tm_g_gh_boxplot(label = "Box Plot", dataname = "ADLB"),
    classes = "pick_delayed"
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Box Plot")
})

test_that("tm_g_gh_boxplot can be created with choices_selected arguments", {
  withr::local_options(lifecycle_verbosity = "quiet")
  module <- tm_g_gh_boxplot(
    label = "Box Plot",
    dataname = "ADLB",
    param_var = "PARAMCD",
    param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
    yaxis_var = choices_selected(c("AVAL", "BASE", "CHG"), "AVAL"),
    xaxis_var = choices_selected(c("AVISITCD", "ACTARM", "ARM"), "AVISITCD"),
    facet_var = choices_selected(c("ARM", "ACTARM"), "ARM"),
    trt_group = choices_selected(c("ARM", "ACTARM"), "ARM")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Box Plot")
})

test_that("tm_g_gh_boxplot can be created with custom picks arguments", {
  module <- tm_g_gh_boxplot(
    label = "Box Plot",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG"), "AVAL"),
    xaxis_var = teal.picks::variables(c("AVISITCD", "ACTARM", "ARM"), "AVISITCD"),
    facet_var = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Box Plot")
})

test_that("tm_g_gh_boxplot handles different parameter combinations", {
  module <- tm_g_gh_boxplot(
    label = "Box Plot - Complex",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "CRP", multiple = FALSE),
      check_dataset = FALSE
    ),
    yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG"), "BASE"),
    xaxis_var = teal.picks::variables(c("AVISITCD", "ACTARM", "ARM"), "ARM"),
    facet_var = teal.picks::variables(c("ARM", "ACTARM", "SEX"), "SEX"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ACTARM"),
    facet_ncol = 2,
    loq_legend = TRUE,
    rotate_xlab = FALSE
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Box Plot - Complex")
})

test_that("tm_g_gh_boxplot accepts custom color and shape specifications", {
  module <- tm_g_gh_boxplot(
    label = "Box Plot - Styled",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG"), "AVAL"),
    xaxis_var = teal.picks::variables(c("AVISITCD", "ACTARM", "ARM"), "ARM"),
    facet_var = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    color_manual = c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C"),
    shape_manual = c("N" = 1, "Y" = 2, "NA" = 0),
    font_size = c(12, 8, 20),
    dot_size = c(2, 1, 12),
    alpha = c(0.8, 0.0, 1.0)
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Box Plot - Styled")
})

test_that("tm_g_gh_boxplot accepts line arguments", {
  module <- tm_g_gh_boxplot(
    label = "Box Plot - With Lines",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG"), "AVAL"),
    xaxis_var = teal.picks::variables(c("AVISITCD", "ACTARM", "ARM"), "ARM"),
    facet_var = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    hline_arb = c(60, 50),
    hline_arb_label = c("Line A", "Line B"),
    hline_arb_color = c("grey", "red"),
    hline_vars = c("ANRHI", "ANRLO"),
    hline_vars_colors = c("pink", "brown"),
    hline_vars_labels = c("ANRHI Label", "ANRLO Label")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Box Plot - With Lines")
})

test_that("tm_g_gh_boxplot handles plot dimensions correctly", {
  module <- tm_g_gh_boxplot(
    label = "Box Plot - Sized",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG"), "AVAL"),
    xaxis_var = teal.picks::variables(c("AVISITCD", "ACTARM", "ARM"), "ARM"),
    facet_var = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    plot_height = c(700, 300, 2000),
    plot_width = c(1000, 500, 2000)
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Box Plot - Sized")
})

describe("tm_g_gh_boxplot: invalid arguments", {
  wrong_params <- list(
    label = 2L,
    dataname = list("A list of strings"),
    param = list("A list of strings"),
    yaxis_var = 1.5,
    xaxis_var = "1.5",
    facet_var = TRUE,
    trt_group = data.frame(),
    color_manual = c(1, 2, 3, 4),
    shape_manual = c("a", "b", "c"),
    facet_ncol = "2",
    loq_legend = "A string",
    rotate_xlab = 1L,
    hline_arb = c("0.02", "0.05"),
    hline_vars = c(0.02, 0.05),
    plot_height = c("600", "300", "2000"),
    plot_width = c("1000", "500", "2000"),
    font_size = c("12", "8", "20"),
    dot_size = c("2", "1", "12"),
    alpha = c("0.8", "0.0", "1.0"),
    pre_output = 123L,
    post_output = list(TRUE),
    transformators = list("not a function")
  )
  for (wrong_param in names(wrong_params)) {
    it(paste0("throws an error when ", wrong_param, " is invalid"), {
      args <- list(label = "module")
      args[[wrong_param]] <- wrong_params[[wrong_param]]
      expect_error(
        suppressWarnings(do.call(tm_g_gh_boxplot, args), classes = "pick_delayed"),
        sprintf("Assertion on '%s' failed", wrong_param)
      )
    })
  }

  wrong_params2 <- list(
    hline_arb_label = c(1, 2),
    hline_arb_color = c(1, 2),
    hline_vars_colors = c(1, 2),
    hline_vars_labels = c(1, 2)
  )
  for (wrong_param2 in names(wrong_params2)) {
    it(paste0("throws an error when ", wrong_param2, " is invalid"), {
      args <- list(
        label = "module",
        hline_arb = c(0.02, 0.05),
        hline_vars = c("ANRHI", "ANRLO")
      )
      args[[wrong_param2]] <- wrong_params2[[wrong_param2]]
      expect_error(
        suppressWarnings(do.call(tm_g_gh_boxplot, args), classes = "pick_delayed"),
        sprintf("Assertion on '%s' failed", wrong_param2)
      )
    })
  }

  deprecated <- c("param_var")
  for (deprecated_param in deprecated) {
    it(paste0("gives a deprecation warning when ", deprecated_param, " is used"), {
      args <- list(label = "module")
      args[[deprecated_param]] <- "PARAMCD"
      lifecycle::expect_deprecated(
        suppressWarnings(do.call(tm_g_gh_boxplot, args), classes = "pick_delayed")
      )
    })
  }

  it("all arguments are tested", {
    expect_setequal(unique(c(names(wrong_params), names(wrong_params2), deprecated)), names(formals(tm_g_gh_boxplot)))
  })
})
