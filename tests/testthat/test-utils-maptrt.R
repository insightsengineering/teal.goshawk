describe("maptrt", {
  adsl <- data.frame(ARMCD = c("ARM A", "ARM B", "ARM A"))

  it("prints the left hand side of the treatment mapping code", {
    expect_output(maptrt(adsl$ARMCD, code = "M"), '"ARM A" = "",')
    expect_output(maptrt(adsl$ARMCD, code = "M"), '"ARM B" = "",')
  })

  it("prints the left hand side of the treatment ordering code", {
    expect_output(maptrt(adsl$ARMCD, code = "O"), 'ARMCD == "ARM A" ~  ,')
    expect_output(maptrt(adsl$ARMCD, code = "O"), 'ARMCD == "ARM B" ~  ,')
  })

  it("throws an error for an unknown code", {
    expect_error(maptrt(adsl$ARMCD, code = "X"), "'arg' should be one of")
  })
})
