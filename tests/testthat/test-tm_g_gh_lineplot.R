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

lineplot_module <- function(...) {
  args <- utils::modifyList(
    list(
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
    ),
    list(...),
    keep.null = TRUE
  )
  suppressMessages(do.call(tm_g_gh_lineplot, args))
}

lineplot_inputs <- function(...) {
  utils::modifyList(
    default_module_inputs(
      stat = "mean",
      include_stat = TRUE,
      relative_height = 1000,
      rotate_xlab = FALSE,
      count_threshold = 0,
      dodge = 0.4,
      plot_font_size = 12,
      dot_size = 2,
      table_font_size = 12
    ),
    list(...)
  )
}

describe("ui_lineplot", {
  it("returns a tag list with the plot and line settings outputs", {
    ui <- module_ui(lineplot_module())
    html <- as.character(ui)
    expect_s3_class(ui, "shiny.tag.list")
    expect_match(html, "test-plot")
    expect_match(html, "test-lines")
    expect_match(html, "test-symbols")
    expect_no_match(html, "test-shape_choices")
  })

  it("adds a line splitting selector when shape_choices are given", {
    expect_match(as.character(module_ui(lineplot_module(shape_choices = c("SEX", "RACE")))), "test-shape_choices")
  })
})

describe("srv_lineplot", {
  mod <- lineplot_module()

  it("creates a line plot with the mean statistic", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs())
      session$elapse(2000)

      expect_true(inherits(plot_r(), c("ggplot", "grob", "gtable")))
      code <- code_of(plot_q())
      expect_match(code, "goshawk::g_lineplot", fixed = TRUE)
      expect_match(code, "median = FALSE", fixed = TRUE)
      expect_match(code, "xtick <- ggplot2::waiver()", fixed = TRUE)
    })
  })

  it("creates a line plot with the median statistic", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs(stat = "median"))
      session$elapse(2000)

      expect_match(code_of(plot_q()), "median = TRUE", fixed = TRUE)
      expect_true(inherits(plot_r(), c("ggplot", "grob", "gtable")))
    })
  })

  it("computes a wider Y-axis zoom range from the confidence intervals than from the quartiles", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs(stat = "mean"))
      mean_range <- data_state()$range
      do.call(session$setInputs, lineplot_inputs(stat = "median"))
      median_range <- data_state()$range

      expect_named(mean_range, c("min", "max"))
      expect_lt(mean_range[["min"]], mean_range[["max"]])
      expect_false(identical(mean_range, median_range))
    })
  })

  it("renders a color and line type input for each treatment arm", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs())
      session$elapse(2000)

      n_arms <- nlevels(as.factor(anl_q()$ANL$ARM))
      html <- output$lines$html
      expect_match(html, sprintf("test-line_color_%d", n_arms))
      expect_match(html, sprintf("test-line_type_%d", n_arms))
      expect_length(line_type_selected(), n_arms)
      expect_true(all(line_type_selected() == "solid"))
    })
  })

  it("uses the user selected line colors and types", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs(line_color_1 = "#123456", line_type_1 = "dashed"))
      session$elapse(2000)

      expect_equal(unname(line_color_selected()[1]), "#123456")
      expect_equal(unname(line_type_selected()[1]), "dashed")
      expect_match(code_of(plot_q()), '"#123456"', fixed = TRUE)
    })
  })

  it("uses colors matching the arm names, then by position, then black", {
    mod <- lineplot_module(color_manual = c("#ff0000", Placebo = "#00ff00"))
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs())
      session$elapse(2000)

      colors <- line_color_selected()
      expect_equal(colors[["Placebo"]], "#00ff00")
      expect_equal(unname(colors[1]), "#ff0000")
      expect_equal(colors[["150mg QD"]], "#000000")
    })
  })

  it("asks to select a line splitting variable before showing symbol settings", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs())
      session$elapse(2000)

      expect_null(symbol_type_selected())
      expect_error(output$symbols, "Please select line splitting variable first.")
    })
  })

  it("renders a symbol input for each level of the line splitting variable", {
    mod <- lineplot_module(shape_choices = c("SEX", "RACE"))
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs())
      .change_selectors(selectors, shape_choices = "SEX")
      session$elapse(2000)

      n_levels <- nlevels(anl_q()$ANL$SEX)
      expect_match(output$symbols$html, sprintf("test-symbol_type_%d", n_levels))
      expect_length(symbol_type_selected(), n_levels)
      expect_equal(unname(symbol_type_selected()[1]), "circle")

      code <- code_of(plot_q())
      expect_match(code, 'shape = "SEX"', fixed = TRUE)
      expect_true(inherits(plot_r(), c("ggplot", "grob", "gtable")))
    })
  })

  it("uses the user selected symbol types", {
    mod <- lineplot_module(shape_choices = c("SEX", "RACE"))
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs(symbol_type_1 = "square"))
      .change_selectors(selectors, shape_choices = "SEX")
      session$elapse(2000)

      expect_equal(unname(symbol_type_selected()[1]), "square")
    })
  })

  it("keeps only the custom X-axis ticks found in the data", {
    mod <- lineplot_module(xtick = c("BL", "W 1", "NOT A VISIT"), xlabel = c("Baseline", "Week 1", "None"))
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs())
      session$elapse(2000)

      q <- plot_q()
      expect_match(code_of(q), "keep_index <- which(", fixed = TRUE)
      expect_identical(q[["xtick"]], c("BL", "W 1"))
      expect_identical(q[["xlabel"]], c("Baseline", "Week 1"))
    })
  })

  it("treats a missing contributing observations threshold as zero", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs(count_threshold = NA))
      session$elapse(2000)

      expect_true(inherits(plot_r(), c("ggplot", "grob", "gtable")))
    })
  })

  it("returns a teal_report with the plot code", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs())
      session$elapse(2000)

      report <- session$getReturned()()
      expect_s4_class(report, "teal_report")
      expect_match(code_of(report), "goshawk::g_lineplot", fixed = TRUE)
    })
  })

  it("fails validation when no biomarker is selected", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, lineplot_inputs())
      param <- selectors$xaxis_param()
      param$values$selected <- character(0)
      selectors$xaxis_param(param)
      expect_error(validated_q(), "Please select a biomarker")
    })
  })
})
