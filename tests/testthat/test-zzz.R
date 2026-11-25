describe(".onLoad: Initialised on package load", {
  testthat::skip_if(getOption("testthat_interactive"))
  it("loads without error", {
    testthat::expect_no_error(.onLoad())
  })

  it("registers the teal.logger handler", {
    expect_equal(
      attr(globalCallingHandlers()$message, "type", exact = TRUE),
      "teal.logger_handler"
    )
  })
})
