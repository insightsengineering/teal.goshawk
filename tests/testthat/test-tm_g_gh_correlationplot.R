local_log_threshold("WARN")

test_that("tm_g_gh_correlationplot can be created with default arguments", {
  module <- suppressWarnings(
    tm_g_gh_correlationplot(label = "Correlation Plot", dataname = "ADLB"),
    classes = "pick_delayed"
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Correlation Plot")
})

test_that("tm_g_gh_correlationplot can be created with choices_selected arguments", {
  withr::local_options(lifecycle_verbosity = "quiet")
  module <- tm_g_gh_correlationplot(
    label = "Correlation Plot",
    dataname = "ADLB",
    param_var = "PARAMCD",
    xaxis_param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
    yaxis_param = choices_selected(c("ALT", "CRP", "IGA"), "CRP"),
    xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
    yaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
    trt_group = choices_selected(c("ARM", "ACTARM"), "ARM")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Correlation Plot")
})

test_that("tm_g_gh_correlationplot can be created with custom picks arguments", {
  module <- tm_g_gh_correlationplot(
    label = "Correlation Plot",
    dataname = "ADLB",
    xaxis_param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    yaxis_param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "CRP", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
    yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Correlation Plot")
})

test_that("tm_g_gh_correlationplot handles multiple parameter combinations", {
  module <- tm_g_gh_correlationplot(
    label = "Correlation Plot - Complex",
    dataname = "ADLB",
    xaxis_param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    yaxis_param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "IGA", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "CHG"),
    yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "PCHG"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ACTARM"),
    facet_ncol = 2,
    visit_facet = TRUE,
    trt_facet = FALSE,
    reg_line = FALSE,
    loq_legend = TRUE,
    rotate_xlab = FALSE
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Correlation Plot - Complex")
})

test_that("tm_g_gh_correlationplot accepts custom color and shape specifications with choices_selected", {
  module <- tm_g_gh_correlationplot(
    label = "Correlation Plot - Styled",
    dataname = "ADLB",
    xaxis_param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    yaxis_param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "CRP", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
    yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    color_manual = c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C"),
    shape_manual = c("N" = 1, "Y" = 2, "NA" = 0),
    font_size = c(12, 8, 20),
    dot_size = c(1, 1, 12),
    reg_text_size = c(3, 3, 10)
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Correlation Plot - Styled")
})

test_that("tm_g_gh_correlationplot accepts line arguments with choices_selected", {
  module <- tm_g_gh_correlationplot(
    label = "Correlation Plot - With Lines",
    dataname = "ADLB",
    xaxis_param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
      check_dataset = FALSE
    ),
    yaxis_param = teal.picks::picks(
      teal.picks::variables("PARAMCD", "PARAMCD"),
      teal.picks::values(c("ALT", "CRP", "IGA"), "CRP", multiple = FALSE),
      check_dataset = FALSE
    ),
    xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
    yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
    trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
    hline_arb = c(40, 50),
    hline_arb_label = "arb hori label",
    hline_arb_color = c("red", "blue"),
    hline_vars = c("ANRHI", "ANRLO"),
    hline_vars_colors = c("green", "blue"),
    hline_vars_labels = c("ANRHI Label", "ANRLO Label"),
    vline_arb = c(50, 70),
    vline_arb_label = "arb vert label",
    vline_arb_color = c("green", "orange"),
    vline_vars = c("ANRHI", "ANRLO"),
    vline_vars_colors = c("yellow", "orange"),
    vline_vars_labels = c("ANRHI Label", "ANRLO Label")
  )

  expect_s3_class(module, "teal_module")
  expect_equal(module$label, "Correlation Plot - With Lines")
})

describe("tm_g_gh_correlationplot: invalid arguments", {
  wrong_params <- list(
    label = 2L,
    dataname = list("A list of strings"),
    xaxis_param = list("A list of strings"),
    yaxis_param = list("A list of strings"),
    xaxis_var = 1.5,
    yaxis_var = "1.5",
    trt_group = TRUE,
    color_manual = c(1, 2, 3, 4),
    shape_manual = c(1, 2, 3, 4),
    facet_ncol = "2",
    visit_facet = "A string",
    trt_facet = 1L,
    reg_line = 12.5,
    loq_legend = "A string",
    rotate_xlab = data.frame(letters = letters),
    hline_arb = c("0.02", "0.05"),
    # hline_arb_color = "red", # own test
    # hline_arb_label = "Horizontal line", # own test
    hline_vars = c(0.02, 0.05),
    # hline_vars_colors = "green", # own test
    # hline_vars_labels = hline_vars, # own test
    vline_arb = c("0.02", "0.05"),
    # vline_arb_color = "red", # own test
    # vline_arb_label = "Vertical line", # own test
    vline_vars = c(0.02, 0.05),
    # vline_vars_colors = "green", # own test
    # vline_vars_labels = vline_vars, # own test
    plot_height = c("600", "300", "2000"),
    plot_width = c("1000", "500", "2000"),
    font_size = c("12", "8", "20"),
    dot_size = c("12", "8", "20"),
    reg_text_size = c("12", "8", "20"),
    pre_output = 123L,
    post_output = list(TRUE),
    transformators = list("not a function")
  )
  for (wrong_param in names(wrong_params)) {
    it(paste0("throws an error when ", wrong_param, " is invalid"), {
      args <- list(label = "module")
      args[[wrong_param]] <- wrong_params[[wrong_param]]
      expect_error(
        suppressWarnings(do.call(tm_g_gh_correlationplot, args), classes = "pick_delayed"),
        sprintf("Assertion on '%s' failed", wrong_param)
      )
    })
  }

  wrong_params2 <- list(
    hline_arb_label = c(1, 2), hline_arb_color = c(1, 2), hline_vars_colors = c(1, 2), hline_vars_labels = c(1, 2),
    vline_arb_label = c(1, 2), vline_arb_color = c(1, 2), vline_vars_colors = c(1, 2), vline_vars_labels = c(1, 2)
  )
  for (wrong_param2 in names(wrong_params2)) {
    it(paste0("throws an error when ", wrong_param2, " is invalid"), {
      args <- list(
        label = "module",
        hline_arb = c(0.02, 0.05),
        vline_arb = c(0.02, 0.05),
        hline_vars = c("ANRHI", "ANRLO"),
        vline_vars = c("ANRHI", "ANRLO")
      )
      args[[wrong_param2]] <- wrong_params2[[wrong_param2]]
      expect_error(
        suppressWarnings(do.call(tm_g_gh_correlationplot, args), classes = "pick_delayed"),
        sprintf("Assertion on '%s' failed", wrong_param2)
      )
    })
  }
})
