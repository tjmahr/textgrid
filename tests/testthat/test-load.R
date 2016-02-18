context("load")

test_that("Load a textgrid file", {
  grid <- TextGrid("assets/silences.TextGrid")
  expect_is(grid, "TextGrid")
})

context("getters")

test_that("Getters on Textgrid", {
  grid <- TextGrid("assets/silences.TextGrid")
  expect_equal(textGridName(grid), "silences.TextGrid")
  expect_equal(startTime(grid), 0)
  expect_equal(size(grid), 1)
  expect_equal(timeUnit(grid), "seconds")
  # Confirm duration within a millisecond
  expect_equal(endTime(grid), .730, tolerance = .001)
})

test_that("Getters on IntervalTier", {
  grid <- TextGrid("assets/silences.TextGrid")
  tier <- grid[["silences"]]

  expect_equal(startTime(tier), 0)
  expect_equal(size(tier), 3)
  expect_equal(tierName(tier), "silences")
  expect_equal(tierNumber(tier), 1)
  expect_equal(size(tier), 3)
  expect_equal(timeUnit(tier), "seconds")
  # Confirm duration within a millisecond
  expect_equal(endTime(tier), .730, tolerance = .001)

  values <- data.frame(tier)$Text
  expect_equal(values, c("sounding", "silent", "sounding"))
})
