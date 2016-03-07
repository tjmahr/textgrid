context("assert")

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



test_that("has_tier", {
  textgrid <- TextGrid(test_path("assets/alignment.TextGrid"))

  expect_true(has_tiers(textgrid, "label"))
  expect_true(has_tiers(textgrid, c("label", "word", "segment")))

  expect_false(has_tiers(textgrid, "null"))
  expect_false(has_tiers(textgrid, letters))
  expect_false(has_tiers(textgrid, c("label", "null")))

  expect_error(
    assertthat::assert_that(has_tiers(textgrid, "null")),
    regexp = "Tier not found in 'alignment.TextGrid': 'null'"
  )

  expect_error(
    assertthat::assert_that(has_tiers(textgrid, c("label", "null1", "null2"))),
    regexp = "Tiers not found in 'alignment.TextGrid': 'null1', 'null2'"
  )
})

test_that("is_interval_tier", {
  textgrid <- TextGrid(test_path("assets/tones.TextGrid"))

  expect_true(is_interval_tier(textgrid, "m"))
  expect_true(is_interval_tier(textgrid, c("m", "j")))

  expect_error(
    assert_that(is_interval_tier(textgrid, "b")),
    "The following tier is not an interval tier in 'tones.TextGrid': 'b'")

  expect_error(
    assert_that(is_interval_tier(textgrid, c("b", "d"))),
    "The following tiers are not interval tiers in 'tones.TextGrid': 'b', 'd'")

})

test_that("is_text_tier", {
  textgrid <- TextGrid(test_path("assets/tones.TextGrid"))

  expect_true(is_text_tier(textgrid, "b"))
  expect_true(is_text_tier(textgrid, c("b", "d")))

  expect_error(
    assert_that(is_text_tier(textgrid, "m")),
    "The following tier is not a text tier in 'tones.TextGrid': 'm'")

  expect_error(
    assert_that(is_text_tier(textgrid, c("m", "j"))),
    "The following tiers are not text tiers in 'tones.TextGrid': 'm', 'j'")
})
