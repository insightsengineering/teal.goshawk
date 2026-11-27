describe(".onLoad: Initialised on package load without error", {
  testthat::skip_if(getOption("testthat_interactive"))
  testthat::expect_no_error(.onLoad())
})
