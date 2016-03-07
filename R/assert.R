

#' Check that tiers are in a textgrid
#' @param textgrid a textgrid object
#' @param tiers names of tiers to check for in the textgrid
#' @return TRUE if the test passes
#' @export
has_tiers <- function(textgrid, tiers) {
  all(tiers %in% tierName(textgrid))
}



# Check that a table is part of a db connection (error message)
on_failure(has_tiers) <- function(call, env) {
  # eval(call$tiers, env) looks up the value of 'tiers' in the environment of
  # the failed function call
  tiers <- eval(call$tiers, env)
  textgrid <- eval(call$textgrid, env)
  textgrid_name <- textGridName(textgrid)

  missing_tiers <- setdiff(tiers, tierName(textgrid))

  sprintf("The tier '%s' does not exist in the '%s'",
          missing_tiers, textgrid_name)
}

# assert_that(has_tiers(textgrid, "n"))
