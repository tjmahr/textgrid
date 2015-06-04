context("load")

test_that("Load a textgrid file", {
  grid <- TextGrid("assets/silences.TextGrid")

  # Confirm duration within a millisecond
  expect_equal(grid@textGridXMin, 0)
  expect_equal(grid@textGridXMax, .730, tolerance = .001)
})
