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
    transformators = list("not a function"),
    color_comb = 1L,
    decorators = 1L
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

  wrong_params2 <- list(hline_arb_label = c(1, 2), hline_arb_color = c(1, 2))
  for (wrong_param2 in names(wrong_params2)) {
    it(paste0("throws an error when ", wrong_param2, " is invalid"), {
      args <- list(
        label = "module",
        hline_arb = c(0.02, 0.05)
      )
      args[[wrong_param2]] <- wrong_params2[[wrong_param2]]
      expect_error(
        suppressWarnings(do.call(tm_g_gh_density_distribution_plot, args), classes = "pick_delayed"),
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
        suppressWarnings(do.call(tm_g_gh_density_distribution_plot, args), classes = "pick_delayed")
      )
    })
  }

  it("all arguments are tested", {
    expect_setequal(
      unique(c(names(wrong_params), names(wrong_params2), deprecated)),
      names(formals(tm_g_gh_density_distribution_plot))
    )
  })
})

density_module <- function(...) {
  args <- utils::modifyList(
    list(
      label = "Density Distribution Plot",
      dataname = "ADLB",
      param = teal.picks::picks(
        teal.picks::variables("PARAMCD", "PARAMCD"),
        teal.picks::values(c("ALT", "CRP", "IGA"), "ALT", multiple = FALSE),
        check_dataset = FALSE
      ),
      xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      trt_group = teal.picks::variables(c("ARM", "ACTARM"), "ARM")
    ),
    list(...)
  )
  suppressMessages(do.call(tm_g_gh_density_distribution_plot, args))
}

density_inputs <- function(...) {
  utils::modifyList(
    default_module_inputs(
      facet_ncol = 2,
      comb_line = TRUE,
      rug_plot = FALSE,
      rotate_xlab = FALSE,
      font_size = 12,
      line_size = 1
    ),
    list(...)
  )
}

describe("ui_g_density_distribution_plot", {
  it("returns a tag list with the plot, table and range zoom inputs", {
    ui <- module_ui(density_module())
    html <- as.character(ui)
    expect_s3_class(ui, "shiny.tag.list")
    expect_match(html, "test-plot")
    expect_match(html, "test-table_ui")
    expect_match(html, "test-xrange_scale")
    expect_match(html, "test-yrange_scale")
  })
})

describe("srv_g_density_distribution_plot", {
  mod <- density_module()

  it("creates a density plot", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, density_inputs())
      session$elapse(2000)

      expect_s3_class(create_plot()[["plot"]], "ggplot")
      expect_match(code_of(create_plot()), "goshawk::g_density_distribution_plot", fixed = TRUE)
      expect_s4_class(create_plot_dims(), "teal_report")
    })
  })

  it("creates and renders a summary table", {
    # `covr` rewrites the summary table code into nested `{` blocks ending with a bare symbol,
    # which `teal.code::eval_code()` (<= 0.7.2) fails to parse.
    skip_on_covr()
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, density_inputs())
      session$elapse(2000)

      expect_s3_class(create_table()[["tbl"]], "data.frame")
      expect_match(code_of(create_table()), "goshawk::t_summarytable", fixed = TRUE)
      expect_type(output$table_ui, "character")
    })
  })

  it("returns a teal_report with the plot and the summary table", {
    # `covr` rewrites the summary table code into nested `{` blocks ending with a bare symbol,
    # which `teal.code::eval_code()` (<= 0.7.2) fails to parse.
    skip_on_covr()
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, density_inputs())
      session$elapse(2000)

      report <- session$getReturned()()
      expect_s4_class(report, "teal_report")
      expect_match(code_of(report), "goshawk::g_density_distribution_plot", fixed = TRUE)
      expect_match(code_of(report), "goshawk::t_summarytable", fixed = TRUE)
      expect_true("### Descriptive Statistics" %in% unlist(teal.reporter::teal_card(report)))
    })
  })

  it("sets the Y-axis zoom from the maximum density per visit and treatment", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, density_inputs())
      session$elapse(2000)

      expect_equal(unname(data_state_y()$range[1]), 0)
      expect_gt(data_state_y()$range[2], 0)
      expect_equal(data_state_y()$step, round(data_state_y()$range[[2]] / 1.2 / 100, 5), tolerance = 1e-4)
      expect_null(data_state_x()$step)
    })
  })

  it("passes the plot settings to the plot", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, density_inputs(rug_plot = TRUE, comb_line = FALSE, line_size = 2))
      session$elapse(2000)

      code <- code_of(create_plot())
      expect_match(code, "rug_plot = TRUE", fixed = TRUE)
      expect_match(code, "comb_line = FALSE", fixed = TRUE)
      expect_match(code, "line_size = 2", fixed = TRUE)
    })
  })

  it("fails validation when no X-axis variable is selected", {
    shiny::testServer(mod$server, args = module_server_args(mod), {
      do.call(session$setInputs, density_inputs())
      .change_selectors(selectors, xaxis_var = character(0))
      expect_error(validated_q(), "Please select an X-Axis variable")
    })
  })
})
