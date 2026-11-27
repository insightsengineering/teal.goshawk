validation_messages <- function(iv) {
  unlist(iv$validate())
}

describe("ui_arbitrary_lines", {
  it("creates value, label and color inputs with the defaults collapsed", {
    html <- as.character(
      ui_arbitrary_lines("lines", c(1, 2), c("a", "b"), c("red", "blue"), title = "My lines")
    )
    expect_match(html, "My lines")
    expect_match(html, 'id="lines-line_arb"[^>]*value="1, 2"')
    expect_match(html, 'id="lines-line_arb_label"[^>]*value="a, b"')
    expect_match(html, 'id="lines-line_arb_color"[^>]*value="red, blue"')
  })
})

describe("srv_arbitrary_lines", {
  it("splits comma separated values, labels and colors", {
    shiny::testServer(srv_arbitrary_lines, args = list(id = "lines"), {
      session$setInputs(line_arb = "1, 2.5", line_arb_label = "a,b", line_arb_color = "red , blue")
      result <- session$getReturned()()
      expect_identical(result$line_arb, c(1, 2.5))
      expect_identical(result$line_arb_label, c("a", "b"))
      expect_identical(result$line_arb_color, c("red", "blue"))
      expect_true(result$iv_r()$is_valid())
    })
  })

  it("returns empty labels and colors when there are no lines", {
    shiny::testServer(srv_arbitrary_lines, args = list(id = "lines"), {
      session$setInputs(line_arb = "", line_arb_label = "a", line_arb_color = "red")
      result <- session$getReturned()()
      expect_length(result$line_arb, 0)
      expect_identical(result$line_arb_label, character(0))
      expect_identical(result$line_arb_color, character(0))
      expect_true(result$iv_r()$is_valid())
    })
  })

  it("defaults to an empty label and a red color", {
    shiny::testServer(srv_arbitrary_lines, args = list(id = "lines"), {
      session$setInputs(line_arb = "1", line_arb_label = "", line_arb_color = "")
      result <- session$getReturned()()
      expect_identical(result$line_arb_label, "")
      expect_identical(result$line_arb_color, "red")
    })
  })

  it("is invalid when the values are not numbers", {
    shiny::testServer(srv_arbitrary_lines, args = list(id = "lines"), {
      session$setInputs(line_arb = "1, a", line_arb_label = "", line_arb_color = "")
      expect_match(
        validation_messages(line_arb_iv <- iv_r()),
        "Arbitrary lines values should be a comma separated list of numbers",
        all = FALSE
      )
      expect_false(line_arb_iv$is_valid())
    })
  })

  it("is invalid when the number of colors does not match the number of lines", {
    shiny::testServer(srv_arbitrary_lines, args = list(id = "lines"), {
      session$setInputs(line_arb = "1, 2, 3", line_arb_label = "", line_arb_color = "red, blue")
      expect_match(
        validation_messages(iv_r()),
        "number of colors should be equal to 1, the number of lines \\(3\\)",
        all = FALSE
      )
    })
  })

  it("is invalid when a color is not an R color", {
    shiny::testServer(srv_arbitrary_lines, args = list(id = "lines"), {
      session$setInputs(line_arb = "1", line_arb_label = "", line_arb_color = "notacolor")
      expect_match(
        validation_messages(iv_r()),
        "cannot be converted to colors in R",
        all = FALSE
      )
    })
  })

  it("is invalid when the number of labels does not match the number of lines", {
    shiny::testServer(srv_arbitrary_lines, args = list(id = "lines"), {
      session$setInputs(line_arb = "1, 2, 3", line_arb_label = "a, b", line_arb_color = "")
      expect_match(
        validation_messages(iv_r()),
        "number of labels should be equal to 1, the number of lines \\(3\\)",
        all = FALSE
      )
    })
  })
})

describe("validate_line_arb_arg", {
  it("accepts a single or one color and label per line", {
    expect_no_error(validate_line_arb_arg(c(1, 2), "red", "label"))
    expect_no_error(validate_line_arb_arg(c(1, 2), c("red", "blue"), c("a", "b")))
    expect_no_error(validate_line_arb_arg(numeric(0), c("red", "blue", "green"), 1))
  })

  it("throws an error when the colors or labels do not match the lines", {
    expect_error(validate_line_arb_arg("1", "red", "a"), "Assertion on '\"1\"' failed")
    expect_error(validate_line_arb_arg(c(1, 2), c("a", "b", "c"), "a"), "c\\(\"a\", \"b\", \"c\"\\)")
    expect_error(validate_line_arb_arg(c(1, 2), "red", c(1, 2)), "c\\(1, 2\\)")
  })
})

describe("validate_line_vars_arg", {
  it("accepts a single or one color and label per variable", {
    expect_no_error(validate_line_vars_arg(c("ANRHI", "ANRLO"), "red", c("high", "low")))
    expect_no_error(validate_line_vars_arg(character(0), 1, 2))
  })

  it("throws an error when the colors or labels do not match the variables", {
    expect_error(validate_line_vars_arg(1, "red", "a"), "Assertion on '1' failed")
    expect_error(validate_line_vars_arg(c("ANRHI", "ANRLO"), c(1, 2), "a"), "c\\(1, 2\\)")
    expect_error(validate_line_vars_arg(c("ANRHI", "ANRLO"), "red", c("a", "b", "c")), "c\\(\"a\", \"b\", \"c\"\\)")
  })
})
