describe("create_picks_helpers", {
  it("returns itself when it is already a pick object with dataset", {
    pick <- create_picks_helper(
      dataset = teal.picks::datasets("else"),
      teal.picks::picks(teal.picks::datasets("dataset"), teal.picks::variables())
    )
    expect_s3_class(pick, "picks")
    expect_equal(pick$dataset, teal.picks::datasets("dataset"))
  })
  it("fills the dataset element when it receives a picks object", {
    pick <- create_picks_helper(
      dataset = teal.picks::datasets("else"),
      teal.picks::picks(teal.picks::variables(), check_dataset = FALSE)
    )
    expect_s3_class(pick, "picks")
    expect_equal(pick$dataset, teal.picks::datasets("else"))
  })
  it("creates a pick object with dataset", {
    pick <- create_picks_helper(
      dataset = teal.picks::datasets("else"),
      teal.picks::datasets("dataset")
    )
    expect_s3_class(pick, "picks")
    expect_equal(pick$dataset, teal.picks::datasets("dataset"))
  })

  it("creates a pick object with dataset and variables", {
    pick <- create_picks_helper(
      dataset = teal.picks::datasets("else"),
      teal.picks::picks(teal.picks::datasets("dataset"), teal.picks::variables())
    )
    expect_s3_class(pick, "picks")
    expect_equal(pick$dataset, teal.picks::datasets("dataset"))
  })

  it("throws an error when arguments are not supported", {
    expect_error(
      create_picks_helper(
        dataset = teal.picks::datasets("else"),
        "unsupported_argument"
      ),
      "Assertion on 'x' failed"
    )
  })
})

describe("migrate_choices_selected_to_variables", {
  it("returns picks objects unchanged", {
    x <- teal.picks::picks(teal.picks::datasets("ADLB", "ADLB"), teal.picks::variables("AVAL", "AVAL"))
    expect_identical(migrate_choices_selected_to_variables(x), x)
  })

  it("returns NULL when NULL is allowed", {
    expect_null(migrate_choices_selected_to_variables(NULL, null.ok = TRUE))
  })

  it("converts choices_selected to variables with a deprecation warning", {
    cs <- teal.transform::choices_selected(c("AVAL", "CHG"), "AVAL")
    lifecycle::expect_deprecated(x <- migrate_choices_selected_to_variables(cs))
    expect_s3_class(x, "variables")
    expect_false(attr(x, "multiple"))
  })

  it("infers multiple selection from choices_selected with several selected values", {
    withr::local_options(lifecycle_verbosity = "quiet")
    cs <- teal.transform::choices_selected(c("AVAL", "CHG"), c("AVAL", "CHG"))
    expect_true(attr(migrate_choices_selected_to_variables(cs), "multiple"))
  })

  it("throws an error when multiple does not match the variables metadata", {
    x <- teal.picks::variables(c("AVAL", "CHG"), "AVAL", multiple = FALSE)
    expect_error(
      migrate_choices_selected_to_variables(x, multiple = TRUE),
      "`multiple` metadata does not match the requirement for x"
    )
  })

  it("throws an error when the object is not a variables object", {
    expect_error(
      migrate_choices_selected_to_variables(teal.picks::values(c("A", "B"))),
      "Assertion on 'teal.picks::values\\(c\\(\"A\", \"B\"\\)\\)' failed"
    )
  })
})

describe("migrate_choices_selected_to_values", {
  it("returns picks and variables objects unchanged", {
    x <- teal.picks::picks(teal.picks::datasets("ADLB", "ADLB"), teal.picks::variables("PARAMCD", "PARAMCD"))
    expect_identical(migrate_choices_selected_to_values(x), x)
    expect_identical(migrate_choices_selected_to_values(x$variables), x$variables)
  })

  it("converts choices_selected to values with a deprecation warning", {
    cs <- teal.transform::choices_selected(c("ALT", "CRP"), "CRP", fixed = TRUE)
    lifecycle::expect_deprecated(x <- migrate_choices_selected_to_values(cs))
    expect_s3_class(x, "values")
    expect_setequal(x$choices, c("ALT", "CRP"))
    expect_identical(x$selected, "CRP")
    expect_false(teal.picks::is_pick_multiple(x))
  })

  it("converts choices_selected without selection to values without selection", {
    withr::local_options(lifecycle_verbosity = "quiet")
    cs <- teal.transform::choices_selected(c("ALT", "CRP"), NULL)
    expect_null(migrate_choices_selected_to_values(cs)$selected)
  })

  it("throws an error for delayed choices_selected", {
    withr::local_options(lifecycle_verbosity = "quiet")
    cs <- teal.transform::choices_selected(teal.transform::variable_choices("ADLB"), "AVAL")
    expect_error(migrate_choices_selected_to_values(cs), "Delayed `choices_selected` objects cannot be coerced")
  })

  it("throws an error when the object is not a values object", {
    expect_error(migrate_choices_selected_to_values("ALT"), "Must inherit from class 'values'")
  })
})

describe("force_pick_selection", {
  it("returns the pick unchanged when the selection type already matches", {
    x <- teal.picks::picks(
      teal.picks::datasets("ADLB", "ADLB"),
      teal.picks::variables("AVAL", "AVAL", multiple = FALSE)
    )
    expect_no_warning(y <- force_pick_selection(x))
    expect_identical(y, x)
  })

  it("forces a single selection with a warning", {
    x <- teal.picks::picks(
      teal.picks::datasets("ADLB", "ADLB"),
      teal.picks::variables(c("AVAL", "CHG"), multiple = TRUE)
    )
    expect_warning(y <- force_pick_selection(x), "`x` accepts only a single variable selection")
    expect_false(teal.picks::is_pick_multiple(y$variables))
  })

  it("forces a multiple selection with a warning", {
    x <- teal.picks::picks(
      teal.picks::datasets("ADLB", "ADLB"),
      teal.picks::variables("AVAL", "AVAL", multiple = FALSE)
    )
    expect_warning(y <- force_pick_selection(x, multiple = TRUE), "accepts only a multiple variable selection")
    expect_true(teal.picks::is_pick_multiple(y$variables))
  })
})

describe(".picks_datanames", {
  it("returns the unique dataset names of all picks", {
    x <- teal.picks::picks(teal.picks::datasets("ADLB", "ADLB"), teal.picks::variables("AVAL", "AVAL"))
    y <- teal.picks::picks(teal.picks::datasets(c("ADSL", "ADLB"), "ADSL"), teal.picks::variables("ARM", "ARM"))
    expect_setequal(.picks_datanames(x, y), c("ADLB", "ADSL"))
  })

  it("returns 'all' when a dataset choice is delayed", {
    x <- teal.picks::picks(teal.picks::datasets("ADLB", "ADLB"), teal.picks::variables("AVAL", "AVAL"))
    y <- teal.picks::picks(teal.picks::datasets(), teal.picks::variables())
    expect_identical(.picks_datanames(x, y), "all")
  })
})
