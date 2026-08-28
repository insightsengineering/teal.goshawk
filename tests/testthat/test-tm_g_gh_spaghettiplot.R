threshold <- logger::log_threshold(namespace = "teal.goshawk")
logger::log_threshold("ERROR", namespace = "teal.goshawk")
withr::defer(logger::log_threshold(threshold, namespace = "teal.goshawk"))

test_that("tm_g_gh_spaghettiplot can be created with default arguments", {
  module <- suppressWarnings(
    tm_g_gh_spaghettiplot(label = "Spaghetti Plot", dataname = "ADLB"),
    classes = "pick_delayed"
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Spaghetti Plot")
})

test_that("tm_g_gh_spaghettiplot can be created with choices_selected arguments", {
  withr::local_options(lifecycle_verbosity = "quiet")
  module <- tm_g_gh_spaghettiplot(
    label = "Spaghetti Plot",
    dataname = "ADLB",
    param_var = "PARAMCD",
    param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
    idvar = "USUBJID",
    xaxis_var = choices_selected(c("AVISITCD", "AVISIT"), "AVISITCD"),
    yaxis_var = choices_selected(c("AVAL", "CHG", "PCHG"), "AVAL"),
    trt_group = choices_selected(c("ARM", "ACTARM"), "ARM")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Spaghetti Plot")
})

test_that("tm_g_gh_spaghettiplot can be created with custom picks arguments", {
  module <- tm_g_gh_spaghettiplot(
    label = "Spaghetti Plot",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    idvar = "USUBJID",
    xaxis_var = teal.picks::variables(c("AVISITCD", "AVISIT"), "AVISITCD"),
    yaxis_var = teal.picks::variables(c("AVAL", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Spaghetti Plot")
})

test_that("tm_g_gh_spaghettiplot handles multiple parameter combinations", {
  module <- tm_g_gh_spaghettiplot(
    label = "Spaghetti Plot - Complex",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "CRP", multiple = FALSE),
      check_dataset = FALSE
    ),
    idvar = "USUBJID",
    xaxis_var = teal.picks::variables(c("AVISITCD", "AVISIT"), "AVISIT"),
    yaxis_var = teal.picks::variables(c("AVAL", "CHG", "PCHG"), "CHG"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ACTARM"),
    facet_ncol = 3,
    free_x = TRUE,
    rotate_xlab = TRUE,
    group_stats = "MEAN"
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Spaghetti Plot - Complex")
})

test_that("tm_g_gh_spaghettiplot accepts custom color specifications with choices_selected", {
  module <- tm_g_gh_spaghettiplot(
    label = "Spaghetti Plot - Styled",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    idvar = "USUBJID",
    xaxis_var = teal.picks::variables(c("AVISITCD", "AVISIT"), "AVISITCD"),
    yaxis_var = teal.picks::variables(c("AVAL", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    man_color = c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C"),
    color_comb = "#39ff14",
    font_size = c(12, 8, 20),
    dot_size = c(2, 1, 12),
    alpha = c(0.8, 0.0, 1.0)
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Spaghetti Plot - Styled")
})

test_that("tm_g_gh_spaghettiplot accepts line arguments with choices_selected", {
  module <- tm_g_gh_spaghettiplot(
    label = "Spaghetti Plot - With Lines",
    dataname = "ADLB",
    param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    idvar = "USUBJID",
    xaxis_var = teal.picks::variables(c("AVISITCD", "AVISIT"), "AVISITCD"),
    yaxis_var = teal.picks::variables(c("AVAL", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    hline_arb = c(40, 50),
    hline_arb_label = c("arb_1", "arb_2"),
    hline_arb_color = c("red", "blue"),
    hline_vars = c("ANRHI", "ANRLO"),
    hline_vars_colors = c("green", "blue"),
    hline_vars_labels = c("ANRHI Label", "ANRLO Label")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Spaghetti Plot - With Lines")
})

describe("tm_g_gh_spaghettiplot: invalid arguments", {
  wrong_params <- list(
    label = 2L,
    dataname = list("A list of strings"),
    param_var_label = 123,
    idvar = TRUE,
    group_stats = c("NONE", "MEAN"),
    param = list("A list of strings"),
    xaxis_var = 1.5,
    yaxis_var = "1.5",
    trt_group = TRUE,
    xaxis_var_level = 123,
    trt_group_level = list("levels"),
    man_color = c(1, 2, 3, 4),
    color_comb = 456,
    hline_arb = c("0.02", "0.05"),
    hline_vars = c(0.02, 0.05),
    facet_ncol = "2",
    free_x = "A string",
    rotate_xlab = 1L,
    plot_height = c("600", "300", "2000"),
    plot_width = c("1000", "500", "2000"),
    font_size = c("12", "8", "20"),
    dot_size = c("12", "8", "20"),
    alpha = c("0.8", "0.0", "1.0"),
    pre_output = 123L,
    post_output = list(TRUE),
    transformators = "not a list",
    decorators = 1L,
    xtick = base::identity,
    xlabel = 1L
  )

  for (wrong_param in names(wrong_params)) {
    it(paste0("throws an error when ", wrong_param, " is invalid"), {
      args <- list(label = "module")
      args[[wrong_param]] <- wrong_params[[wrong_param]]
      expect_error(
        suppressWarnings(do.call(tm_g_gh_spaghettiplot, args), classes = "pick_delayed"),
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
        suppressWarnings(do.call(tm_g_gh_spaghettiplot, args), classes = "pick_delayed"),
        sprintf("Assertion on '%s' failed", wrong_param2)
      )
    })
  }

  deprecated <- c("param_var", "filter_var")
  for (deprecated_param in deprecated) {
    it(paste0("gives a deprecation warning when ", deprecated_param, " is used"), {
      args <- list(label = "module")
      args[[deprecated_param]] <- "PARAMCD"
      lifecycle::expect_deprecated(
        suppressWarnings(do.call(tm_g_gh_spaghettiplot, args), classes = "pick_delayed")
      )
    })
  }

  it("all arguments are tested", {
    expect_setequal(
      unique(c(names(wrong_params), names(wrong_params2), deprecated)),
      names(formals(tm_g_gh_spaghettiplot))
    )
  })
})
