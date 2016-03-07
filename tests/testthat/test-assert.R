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
  expect_false(has_tiers(textgrid, c("label", "null")))

  expect_error(
    assertthat::assert_that(has_tiers(textgrid, "null")),
    regexp = "The tier 'null' does not exist"
    )

})
