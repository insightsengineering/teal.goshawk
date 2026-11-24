
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
