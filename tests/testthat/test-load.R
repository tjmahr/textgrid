context("load")

test_path <- function(...) {
  if (in_testing_dir(".")) {
    path <- file.path(...)
  } else {
    base <- "tests/testthat"
    if (!file.exists(base)) {
      stop("Can't find `tests/testthat/` in current directory.",
           call. = FALSE)
    }
    path <- file.path(base, ...)
  }

  if (!file.exists(path)) {
    stop("`", path, "` doesn't exist", call. = FALSE)
  }

  path
}

in_testing_dir <- function(path) {
  path <- normalizePath(path)

  if (basename(path) != "testthat") return(FALSE)

  parent <- dirname(path)
  basename(parent) == "tests"
}


test_that("Load a textgrid file", {
  grid <- TextGrid(test_path("assets/silences.TextGrid"))
  expect_is(grid, "TextGrid")
})

context("getters")

test_that("Getters on Textgrid", {
  grid <- TextGrid(test_path("assets/silences.TextGrid"))
  expect_equal(textGridName(grid), "silences.TextGrid")
  expect_equal(tierName(grid), "silences")
  expect_equal(startTime(grid), 0)
  expect_equal(size(grid), 1)
  expect_equal(timeUnit(grid), "seconds")
  # Confirm duration within a millisecond
  expect_equal(endTime(grid), .730, tolerance = .001)

  grid2 <- TextGrid("assets/alignment.TextGrid")
  expect_equal(tierName(grid2), c("word", "label", "segment"))

})

test_that("Getters on IntervalTier", {
  grid <- TextGrid(test_path("assets/silences.TextGrid"))
  tier <- grid[["silences"]]

  expect_equal(startTime(tier), 0)
  expect_equal(size(tier), 3)
  expect_equal(tierType(tier), "IntervalTier")
  expect_equal(tierName(tier), "silences")
  expect_equal(tierNumber(tier), 1)
  expect_equal(size(tier), 3)
  expect_equal(timeUnit(tier), "seconds")
  # Confirm duration within a millisecond
  expect_equal(endTime(tier), .730, tolerance = .001)

  values <- data.frame(tier)$Text
  expect_equal(values, c("sounding", "silent", "sounding"))
})
