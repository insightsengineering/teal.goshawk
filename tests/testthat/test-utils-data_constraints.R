constraint_data <- function() {
  adlb <- data.frame(
    USUBJID = rep(c("S1", "S2", "S3", "S4"), each = 3),
    PARAMCD = rep(c("ALT", "ALT", "CRP"), 4),
    AVISITCD = rep(c("SCR", "BL", "BL"), 4),
    ARM = rep(c("A", "B"), each = 6),
    BASE2 = c(10, 10, 1, 20, 20, 1, 30, 30, 1, 40, 40, 1),
    BASE = c(1.2345, 1.2345, 5, 2, 2, 5, 3, 3, 5, NA, NA, 5),
    AVAL = 1:12
  )
  teal.data::teal_data(ADLB = adlb)
}

# Minimal module server to exercise the data constraint helpers
constraint_server <- function(id, data, param = "ALT", param_var = "PARAMCD", min_rows = 1) {
  shiny::moduleServer(id, function(input, output, session) {
    anl_q_output <- constr_anl_q(
      session, input,
      data = shiny::reactive(data),
      dataname = "ADLB",
      param_r = shiny::reactive(param),
      param_var_r = shiny::reactive(param_var),
      trt_group_r = shiny::reactive("ARM"),
      min_rows = min_rows
    )
    anl_q <- anl_q_output()$value
    iv_r <- anl_q_output()$iv_r
    keep_data_const_opts_updated(session, input, anl_q, shiny::reactive(param))
  })
}

constraint_inputs <- function(...) {
  utils::modifyList(list(constraint_var = "NONE", constraint_range_min = 0, constraint_range_max = 0), list(...))
}

describe("templ_ui_constraint", {
  it("creates the constraint variable selector and range inputs", {
    html <- as.character(templ_ui_constraint(shiny::NS("test"), label = "My Constraint"))
    expect_match(html, "My Constraint")
    expect_match(html, 'id="test-constraint_var"')
    expect_match(html, 'id="test-constraint_range_min"')
    expect_match(html, 'id="test-constraint_range_max"')
    expect_match(html, 'id="test-all_na"')
  })
})

describe("constr_anl_q", {
  it("filters the analysis data on the selected parameter", {
    shiny::testServer(constraint_server, args = list(id = "test", data = constraint_data()), {
      do.call(session$setInputs, constraint_inputs())
      expect_identical(unique(anl_q()$ANL$PARAMCD), "ALT")
      expect_identical(nrow(anl_q()$ANL), 8L)
      expect_match(code_of(anl_q()$qenv), 'dplyr::filter(PARAMCD == "ALT")', fixed = TRUE)
      expect_true(iv_r()$is_valid())
    })
  })

  it("keeps the subjects within the constraint range and those with missing values only", {
    shiny::testServer(constraint_server, args = list(id = "test", data = constraint_data()), {
      do.call(
        session$setInputs,
        constraint_inputs(constraint_var = "BASE", constraint_range_min = 1, constraint_range_max = 2)
      )
      expect_setequal(unique(anl_q()$ANL$USUBJID), c("S1", "S2", "S4"))
      expect_match(code_of(anl_q()$qenv), "filtered_usubjids", fixed = TRUE)
    })
  })

  it("fails validation when the parameter variable is not PARAMCD", {
    shiny::testServer(
      constraint_server,
      args = list(id = "test", data = constraint_data(), param_var = "PARAM"),
      {
        do.call(session$setInputs, constraint_inputs())
        expect_error(anl_q(), "param_var must be 'PARAMCD'")
      }
    )
  })

  it("fails validation when there are not enough rows", {
    shiny::testServer(constraint_server, args = list(id = "test", data = constraint_data(), min_rows = 100), {
      do.call(session$setInputs, constraint_inputs())
      expect_error(anl_q(), "Minimum number of records not met")
    })
  })

  it("is invalid when the constraint minimum is larger than the maximum", {
    shiny::testServer(constraint_server, args = list(id = "test", data = constraint_data()), {
      do.call(
        session$setInputs,
        constraint_inputs(constraint_var = "BASE", constraint_range_min = 5, constraint_range_max = 1)
      )
      messages <- unlist(iv_r()$validate())
      expect_match(messages, "constraint min needs to be less than max", all = FALSE)
      expect_false(iv_r()$is_valid())
    })
  })

  it("is invalid when the constraint range is missing", {
    shiny::testServer(constraint_server, args = list(id = "test", data = constraint_data()), {
      do.call(
        session$setInputs,
        constraint_inputs(constraint_var = "BASE", constraint_range_min = NA, constraint_range_max = NA)
      )
      expect_match(unlist(iv_r()$validate()), "A contraint minimum value is required", all = FALSE)
    })
  })

  it("updates the range inputs with the screening values", {
    min_max <- NULL
    testthat::local_mocked_bindings(update_min_max = function(session, args) min_max <<- args)
    shiny::testServer(constraint_server, args = list(id = "test", data = constraint_data()), {
      do.call(session$setInputs, constraint_inputs(constraint_var = "BASE2"))
      expect_identical(min_max$min, list(label = "Min (10)", min = 10, max = 40, value = 10))
      expect_identical(min_max$max, list(label = "Max (40)", min = 10, max = 40, value = 40))
    })
  })

  it("rounds the baseline range outwards to three decimals", {
    min_max <- NULL
    testthat::local_mocked_bindings(update_min_max = function(session, args) min_max <<- args)
    shiny::testServer(constraint_server, args = list(id = "test", data = constraint_data()), {
      do.call(session$setInputs, constraint_inputs(constraint_var = "BASE"))
      expect_identical(min_max$min$min, 1.234)
      expect_identical(min_max$max$max, 3)
    })
  })

  it("resets the range inputs when all constraint values are missing", {
    min_max <- NULL
    testthat::local_mocked_bindings(update_min_max = function(session, args) min_max <<- args)
    data <- within(constraint_data(), ADLB$BASE2 <- NA_real_)
    shiny::testServer(constraint_server, args = list(id = "test", data = data), {
      do.call(session$setInputs, constraint_inputs(constraint_var = "BASE2"))
      expect_identical(min_max$min, list(label = "Min", min = 0, max = 0, value = 0))
      expect_identical(min_max$max, list(label = "Max", min = 0, max = 0, value = 0))
    })
  })

  it("resets the range inputs when no constraint is selected", {
    min_max <- NULL
    testthat::local_mocked_bindings(update_min_max = function(session, args) min_max <<- args)
    shiny::testServer(constraint_server, args = list(id = "test", data = constraint_data()), {
      do.call(session$setInputs, constraint_inputs(constraint_var = "BASE2"))
      do.call(session$setInputs, constraint_inputs(constraint_var = "NONE"))
      expect_identical(min_max$min, list(label = "Min", min = 0, max = 0, value = 0))
    })
  })

  it("does not update the range inputs when the constraint visit is not in the data", {
    min_max <- NULL
    testthat::local_mocked_bindings(update_min_max = function(session, args) min_max <<- args)
    data <- within(constraint_data(), ADLB <- ADLB[ADLB$AVISITCD != "SCR", ])
    shiny::testServer(constraint_server, args = list(id = "test", data = data), {
      do.call(session$setInputs, constraint_inputs(constraint_var = "BASE2"))
      expect_null(min_max)
    })
  })
})

describe("keep_data_const_opts_updated", {
  it("offers only the constraints with non-missing values", {
    choices <- NULL
    testthat::local_mocked_bindings(
      updateRadioButtons = function(session, inputId, choices) choices <<- choices # nolint: object_name_linter.
    )
    data <- within(constraint_data(), ADLB$BASE2 <- NA_real_)
    shiny::testServer(constraint_server, args = list(id = "test", data = data), {
      do.call(session$setInputs, constraint_inputs())
      expect_identical(choices, c("None" = "NONE", "Baseline" = "BASE"))
    })
  })

  it("offers screening and baseline constraints when they have values", {
    choices <- NULL
    testthat::local_mocked_bindings(
      updateRadioButtons = function(session, inputId, choices) choices <<- choices # nolint: object_name_linter.
    )
    shiny::testServer(constraint_server, args = list(id = "test", data = constraint_data()), {
      do.call(session$setInputs, constraint_inputs())
      expect_identical(choices, c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE"))
    })
  })

  it("offers only 'None' when there are no constraint values", {
    choices <- NULL
    testthat::local_mocked_bindings(
      updateRadioButtons = function(session, inputId, choices) choices <<- choices # nolint: object_name_linter.
    )
    data <- within(constraint_data(), {
      ADLB$BASE2 <- NA_real_
      ADLB$BASE <- NA_real_
    })
    shiny::testServer(constraint_server, args = list(id = "test", data = data), {
      do.call(session$setInputs, constraint_inputs())
      expect_identical(choices, c("None" = "NONE"))
    })
  })
})

describe("formatted_data_constraint", {
  it("describes the screening and baseline constraints with their range", {
    expect_identical(formatted_data_constraint("BASE2", 1, 2), "Data Constraint: Screening from 1 to 2")
    expect_identical(formatted_data_constraint("BASE", 0, 10), "Data Constraint: Baseline from 0 to 10")
  })

  it("describes other constraints as 'None'", {
    expect_identical(formatted_data_constraint("NONE", 1, 2), "Data Constraint: None")
  })
})
