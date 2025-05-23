app_driver <- shinytest2::AppDriver$new(
  shiny::shinyApp(
    ui = shiny::fluidPage(id = "test", "testing"),
    server = function(input, output, session) {
    }
  )
)


testthat::test_that("app says testing", {
  testthat::expect_equal(app_driver$get_text("#test"), "testing")
})
