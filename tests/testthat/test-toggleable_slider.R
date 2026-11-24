describe("toggle_slider_ui", {
  it("creates a toggle button and an output for the inputs", {
    html <- as.character(toggle_slider_ui("slider", "Y-Axis Range Zoom"))
    expect_match(html, "Y-Axis Range Zoom")
    expect_match(html, 'id="slider-toggle"')
    expect_match(html, 'id="slider-inputs"')
  })
})

describe("toggle_slider_server", {
  data_state <- function() shiny::reactiveVal(list(range = c(min = 0, max = 100), step = 1))

  it("initializes the state from the data range and shows a slider", {
    shiny::testServer(toggle_slider_server, args = list(id = "slider", data_state = data_state()), {
      session$setInputs(toggle = 0)
      expect_equal(state$min, 0, ignore_attr = TRUE)
      expect_equal(state$max, 100, ignore_attr = TRUE)
      expect_equal(state$value, c(0, 100), ignore_attr = TRUE)
      expect_true(slider_shown())
      expect_match(output$inputs$html, "js-range-slider")
    })
  })

  it("shows numeric inputs after toggling", {
    shiny::testServer(toggle_slider_server, args = list(id = "slider", data_state = data_state()), {
      session$setInputs(toggle = 1)
      expect_false(slider_shown())
      expect_match(output$inputs$html, 'id="slider-value_low"')
      expect_match(output$inputs$html, 'id="slider-value_high"')
    })
  })

  it("updates the value from the slider after the debounce", {
    shiny::testServer(toggle_slider_server, args = list(id = "slider", data_state = data_state()), {
      session$setInputs(toggle = 0, slider = c(10, 50))
      session$elapse(1000)
      expect_equal(state$value, c(10, 50))
      expect_equal(state$max, 100, ignore_attr = TRUE)
    })
  })

  it("updates the value and the range from the numeric inputs after the debounce", {
    shiny::testServer(toggle_slider_server, args = list(id = "slider", data_state = data_state()), {
      session$setInputs(toggle = 1)
      session$elapse(1000)
      session$setInputs(value_low = -10, value_high = 200)
      session$elapse(1000)
      expect_equal(state$value, c(-10, 200))
      expect_equal(state$min, -10)
      expect_equal(state$max, 200)
    })
  })

  it("resets the state when the data range changes", {
    ds <- data_state()
    shiny::testServer(toggle_slider_server, args = list(id = "slider", data_state = ds), {
      session$setInputs(toggle = 0)
      ds(list(range = c(min = 5, max = 6), step = 0.1))
      session$flushReact()
      expect_equal(state$value, c(5, 6), ignore_attr = TRUE)
    })
  })
})

describe("get_data_range_states", {
  anl <- data.frame(
    PARAMCD = c("ALT", "ALT", "ALT", "ALT", "CRP"),
    AVAL = c(1.2, 3.7, 2, NA, 100),
    AVISITCD = c("BL", "BL", "W 1", "W 1", "BL"),
    ARM = c("A", "B", "A", "B", "A")
  )

  it("returns the rounded range of the variable for the selected parameter", {
    result <- get_data_range_states("AVAL", "ALT", anl)
    expect_identical(result$range, c(min = 1, max = 4))
    expect_null(result$step)
  })

  it("returns a zero range when all values are missing", {
    anl$AVAL <- NA_real_
    expect_identical(get_data_range_states("AVAL", "ALT", anl)$range, c(min = 0, max = 0))
  })

  it("returns the density range and step when a treatment group is given", {
    anl <- data.frame(
      PARAMCD = "ALT",
      AVAL = c(1, 2, 3, 4, 2, 2.5, 3, 3.5),
      AVISITCD = rep(c("BL", "W 1"), each = 4),
      ARM = rep(c("A", "A", "B", "B"), 2)
    )
    result <- get_data_range_states("AVAL", "ALT", anl, trt_group = "ARM")
    expect_identical(result$range[["min"]], 0)
    expect_gt(result$range[["max"]], 0)
    expect_equal(result$step, round(result$range[["max"]] / 1.2 / 100, 5), tolerance = 1e-4)
  })

  it("fails validation when the variable or the parameter are not selected", {
    expect_error(get_data_range_states(NULL, "ALT", anl), "Please select variable")
    expect_error(get_data_range_states("AVAL", NULL, anl), "Please select variable")
  })

  it("fails validation when the variable does not exist", {
    expect_error(get_data_range_states("CHG", "ALT", anl), "variable CHG does not exist")
  })
})
