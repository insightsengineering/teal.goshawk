plot_report <- function() {
  within(teal.reporter::teal_report(), {
    plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
      ggplot2::geom_point()
    plot
  })
}

describe("set_chunk_attrs", {
  it("sets the attributes of the last chunk output", {
    card <- set_chunk_attrs(teal.reporter::teal_card(plot_report()), list(dev.width = 200, dev.height = 100))
    last <- card[[length(card)]]
    expect_identical(attr(last, "dev.width"), 200)
    expect_identical(attr(last, "dev.height"), 100)
  })

  it("warns and returns the card unchanged when the last element is not a chunk output", {
    card <- c(teal.reporter::teal_card(plot_report()), "## Text")
    expect_warning(
      result <- set_chunk_attrs(card, list(dev.width = 200)),
      "The last element of the `teal_card` is not a `chunk_output` object"
    )
    expect_identical(result, card)
    expect_no_warning(set_chunk_attrs(card, list(dev.width = 200), quiet = TRUE))
  })

  it("stops with a warning at the first element that is not a chunk output", {
    card <- teal.reporter::teal_card(plot_report())
    expect_warning(
      result <- set_chunk_attrs(card, list(dev.width = 200), n = 2),
      "The 2 to last element of the `teal_card` is not a `chunk_output` object"
    )
    expect_identical(attr(result[[length(result)]], "dev.width"), 200)
    expect_no_warning(set_chunk_attrs(card, list(dev.width = 200), n = 2, quiet = TRUE))
  })

  it("only modifies chunk outputs with the given inner classes", {
    card <- teal.reporter::teal_card(plot_report())
    result <- set_chunk_attrs(card, list(dev.width = 200), inner_classes = "gt_tbl")
    expect_null(attr(result[[length(result)]], "dev.width"))

    result <- set_chunk_attrs(card, list(dev.width = 200), inner_classes = "recordedplot")
    expect_identical(attr(result[[length(result)]], "dev.width"), 200)
  })
})

describe("set_chunk_dims", {
  it("sets the plot dimensions on the last chunk output", {
    pws <- list(dim = shiny::reactive(list(width = 300, height = 150)))
    q <- shiny::isolate(set_chunk_dims(pws, shiny::reactive(plot_report()))())
    card <- teal.reporter::teal_card(q)
    expect_identical(attr(card[[length(card)]], "dev.width"), 300)
    expect_identical(attr(card[[length(card)]], "dev.height"), 150)
  })

  it("ignores 'auto' dimensions", {
    pws <- list(dim = shiny::reactive(list("auto", "auto")))
    q <- shiny::isolate(set_chunk_dims(pws, shiny::reactive(plot_report()))())
    card <- teal.reporter::teal_card(q)
    expect_null(attr(card[[length(card)]], "dev.width"))
    expect_null(attr(card[[length(card)]], "dev.height"))
  })
})
