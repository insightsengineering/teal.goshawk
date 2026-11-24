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
    transformators = list("not a function"),
    decorators = 1L
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

  deprecated <- c("param_var")
  for (deprecated_param in deprecated) {
    it(paste0("gives a deprecation warning when ", deprecated_param, " is used"), {
      args <- list(label = "module")
      args[[deprecated_param]] <- "PARAMCD"
      lifecycle::expect_deprecated(
        suppressWarnings(do.call(tm_g_gh_correlationplot, args), classes = "pick_delayed")
      )
    })
  }

  it("all arguments are tested", {
    expect_setequal(
      unique(c(names(wrong_params), names(wrong_params2), deprecated)),
      names(formals(tm_g_gh_correlationplot))
    )
  })
})

correlationplot_module <- function(...) {
  args <- utils::modifyList(
    list(
      label = "Correlation Plot",
      dataname = "ADLB",
      xaxis_param = teal.picks::picks(
        teal.picks::variables("PARAMCD", "PARAMCD"),
        teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
        check_dataset = FALSE
      ),
      xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
      yaxis_param = teal.picks::picks(
        teal.picks::variables("PARAMCD", "PARAMCD"),
        teal.picks::values(c("ALT", "CRP", "IGA"), "CRP", multiple = FALSE),
        check_dataset = FALSE
      ),
      yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM"),
      hline_vars = c("ANRHI", "ANRLO"),
      hline_vars_colors = c("pink", "brown"),
      vline_vars = c("ANRHI", "ANRLO"),
      vline_vars_colors = c("yellow", "orange")
    ),
    list(...)
  )
  suppressMessages(do.call(tm_g_gh_correlationplot, args))
}

correlationplot_inputs <- function(...) {
  utils::modifyList(
    default_module_inputs(
      facet_ncol = 2,
      trt_facet = FALSE,
      visit_facet = TRUE,
      reg_line = FALSE,
      loq_legend = TRUE,
      rotate_xlab = FALSE,
      font_size = 12,
      dot_size = 2,
      reg_text_size = 3,
      `vline_arb-line_arb` = "",
      `vline_arb-line_arb_label` = "",
      `vline_arb-line_arb_color` = ""
    ),
    list(...)
  )
}

describe("ui_g_correlationplot", {
  it("returns a tag list with the plot and brushed data outputs", {
    ui <- module_ui(correlationplot_module())
    html <- as.character(ui)
    expect_s3_class(ui, "shiny.tag.list")
    expect_match(html, "test-plot")
    expect_match(html, "test-brush_data")
    expect_match(html, "test-xrange_scale")
    expect_match(html, "test-yrange_scale")
  })

  it("adds range line selectors only when hline_vars and vline_vars are given", {
    html <- as.character(module_ui(correlationplot_module()))
    expect_match(html, "test-hline_vars")
    expect_match(html, "test-vline_vars")

    html <- as.character(module_ui(correlationplot_module(hline_vars = character(0), vline_vars = character(0))))
    expect_no_match(html, "test-hline_vars")
    expect_no_match(html, "test-vline_vars")
  })
})

describe("srv_g_correlationplot", {
  mod <- correlationplot_module()

  it("creates a correlation plot from the transposed data", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs())
      session$elapse(2000)

      expect_s4_class(validated_q(), "teal_data")
      expect_s3_class(data_transpose_q()[["ANL_TRANSPOSED"]], "data.frame")
      expect_true(all(c("BASE_ALT", "AVAL_CRP", "LOQFL_COMB") %in% names(data_transpose_q()[["ANL_TRANSPOSED"]])))
      expect_s3_class(plot_r(), "ggplot")
      expect_match(code_of(plot_q()), "goshawk::g_correlationplot", fixed = TRUE)
    })
  })

  it("derives the LOQ flag from baseline when the X-axis variable is BASE", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs())
      session$elapse(2000)

      expect_match(code_of(data_transpose_q()), 'LOQFL = .data[["LOQFL"]][.data[["AVISITCD"]] == "BL"]', fixed = TRUE)
    })
  })

  it("sets the LOQ flag to 'N' when the axis variables are neither AVAL nor BASE", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs())
      .change_selectors(selectors, xaxis_var = "CHG", yaxis_var = "CHG")
      session$elapse(2000)

      expect_match(code_of(data_transpose_q()), 'ANL_x <- ANL_x %>% dplyr::mutate(LOQFL = "N")', fixed = TRUE)
      expect_match(code_of(data_transpose_q()), 'ANL_y <- ANL_y %>% dplyr::mutate(LOQFL = "N")', fixed = TRUE)
      expect_s3_class(plot_r(), "ggplot")
    })
  })

  it("derives the Y-axis LOQ flag from baseline when the Y-axis variable is BASE", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs())
      .change_selectors(selectors, xaxis_var = "AVAL", yaxis_var = "BASE")
      session$elapse(2000)

      expect_match(code_of(data_transpose_q()), "ANL_y <- ANL_y %>% dplyr::group_by", fixed = TRUE)
      expect_s3_class(plot_r(), "ggplot")
    })
  })

  it("includes the analysis units in the plot labels", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs())
      session$elapse(2000)

      expect_match(plot_labels()$title_text, "\\(.+\\) and .+ \\(.+\\) @ Visits")
      expect_match(plot_labels()$xaxis_lab, "BASE Values$")
    })
  })

  it("omits the units from the plot labels when they are missing", {
    data <- within(get_cached_test_data(), ADLB$AVALU <- "")
    shiny::testServer(mod$server, args = module_server_args(mod, data), {
      do.call(session$setInputs, correlationplot_inputs())
      session$elapse(2000)

      expect_match(plot_labels()$title_text, "^[^()]+ and [^()]+ @ Visits$")
      expect_match(plot_labels()$yaxis_lab, "AVAL Values$")
    })
  })

  it("passes arbitrary horizontal and vertical lines to the plot", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(
        session$setInputs,
        correlationplot_inputs(
          `hline_arb-line_arb` = "10",
          `hline_arb-line_arb_label` = "h",
          `vline_arb-line_arb` = "20, 30",
          `vline_arb-line_arb_label` = "v",
          `vline_arb-line_arb_color` = "blue"
        )
      )
      session$elapse(2000)

      code <- code_of(plot_q())
      expect_match(code, "hline_arb = 10", fixed = TRUE)
      expect_match(code, 'hline_arb_color = "red"', fixed = TRUE)
      expect_match(code, "vline_arb = c(20, 30)", fixed = TRUE)
      expect_match(code, 'vline_arb_color = "blue"', fixed = TRUE)
    })
  })

  it("returns the brushed data and renders it in a table", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs())
      session$elapse(2000)

      expect_s3_class(reactive_df(), "data.frame")
      expect_true(all(c("USUBJID", "ARM", "AVISITCD", "LOQFL_COMB") %in% names(reactive_df())))
      expect_type(output$brush_data, "character")
    })
  })

  it("returns a teal_report with the plot code", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs())
      session$elapse(2000)

      expect_s4_class(decorated_plot_q(), "teal_report")
      expect_match(code_of(decorated_plot_q()), "goshawk::g_correlationplot", fixed = TRUE)
    })
  })

  it("updates the constraint range from the baseline values", {
    min_max <- NULL
    testthat::local_mocked_bindings(update_min_max = function(session, args) min_max <<- args)
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs(constraint_var = "BASE"))
      session$flushReact()

      anl <- dplyr::filter(validated_q()[["ADLB"]], PARAMCD == "ALT", AVISITCD == "BL")
      expect_equal(min_max$min$min, floor(min(anl$BASE, na.rm = TRUE) * 1000) / 1000)
      expect_equal(min_max$max$max, ceiling(max(anl$BASE, na.rm = TRUE) * 1000) / 1000)
      expect_match(min_max$min$label, "^Min \\(")
    })
  })

  it("resets the constraint range when the constraint values are all missing", {
    min_max <- NULL
    testthat::local_mocked_bindings(update_min_max = function(session, args) min_max <<- args)
    data <- within(get_cached_test_data(), ADLB$BASE <- NA_real_)
    shiny::testServer(mod$server, args = module_server_args(mod, data), {
      do.call(session$setInputs, correlationplot_inputs(constraint_var = "BASE"))
      session$flushReact()

      expect_identical(min_max$min, list(label = "Min", min = 0, max = 0, value = 0))
      expect_identical(min_max$max, list(label = "Max", min = 0, max = 0, value = 0))
    })
  })

  it("resets the constraint range when no constraint is selected", {
    min_max <- NULL
    testthat::local_mocked_bindings(update_min_max = function(session, args) min_max <<- args)
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs(constraint_var = "BASE"))
      do.call(session$setInputs, correlationplot_inputs(constraint_var = "NONE"))
      session$flushReact()

      expect_identical(min_max$min, list(label = "Min", min = 0, max = 0, value = 0))
    })
  })

  it("filters the analysis data on the selected constraint range", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs())
      session$elapse(2000)
      do.call(
        session$setInputs,
        correlationplot_inputs(constraint_var = "BASE", constraint_range_min = 0, constraint_range_max = 1000)
      )
      session$elapse(2000)

      expect_match(code_of(data_transpose_q()), "filtered_usubjids", fixed = TRUE)
    })
  })

  it("fails validation when the constraint minimum is larger than the maximum", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(
        session$setInputs,
        correlationplot_inputs(constraint_var = "BASE", constraint_range_min = 10, constraint_range_max = 1)
      )
      expect_error(data_transpose_q(), "constraint min needs to be less than max")
    })
  })

  it("fails validation when the number of plots per row is not a positive integer", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs(facet_ncol = 1.5))
      expect_error(validated_q(), "Please select a facet column integer that is greater than 0")
    })
  })

  it("fails validation when no X-axis variable is selected", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, correlationplot_inputs())
      .change_selectors(selectors, xaxis_var = character(0))
      expect_error(validated_q(), "Please select an X-Axis variable")
    })
  })
})
