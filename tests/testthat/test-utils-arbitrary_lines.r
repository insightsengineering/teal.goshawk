test_that("with_tooltip fails on bad args", {

  expect_error(
    with_tooltip(shiny::textInput("id", "label"))
  )

    expect_error(
      with_tooltip(
        shiny::textInput("id", "label"),
        title = "title",
        placement = "front"
      )
  )

      expect_error(
      with_tooltip(
        shiny::textInput("id", "label"),
        title = "title",
        placement = c("front", "back")
      )
  )

})
