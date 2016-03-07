

#' Check that tiers are in a textgrid
#' @param textgrid a textgrid object
#' @param tiers names of tiers in the textgrid
#' @return TRUE if the test passes
#' @export
has_tiers <- function(textgrid, tiers) {
  all(tiers %in% tierName(textgrid))
}

on_failure(has_tiers) <- function(call, env) {
  # eval(call$tiers, env) looks up the value of 'tiers' in the environment of
  # the failed function call
  tiers <- eval(call$tiers, env)
  textgrid <- eval(call$textgrid, env)
  textgrid_name <- textGridName(textgrid)

  missing_tiers <- setdiff(tiers, tierName(textgrid))

  if (length(missing_tiers) == 1) {
    tier_count <- "Tier"
    tier_name <- sprintf("'%s'", missing_tiers)
  }

  if (1 < length(missing_tiers)) {
    tier_count <- "Tiers"
    tier_name <- sprintf("'%s'", missing_tiers)
    tier_name <- paste(tier_name, collapse = ", ")
  }

  sprintf("%s not found in '%s': %s",
          tier_count, textgrid_name, tier_name)
}




is_tier_type <- function(textgrid, tiers, tier_type) {
  assert_that(has_tiers(textgrid, tiers))
  all(tierType(textgrid[tiers]) == tier_type)
}

# Check that a table is part of a db connection (error message)
on_failure(is_tier_type) <- function(call, env) {
  # eval(call$tiers, env) looks up the value of 'tiers' in the environment of
  # the failed function call
  tiers <- eval(call$tiers, env)
  textgrid <- eval(call$textgrid, env)
  tier_type <- eval(call$tier_type, env)
  textgrid_name <- textGridName(textgrid)

  # wrong_type
  subgrid <- textgrid[tiers]
  wrong_type <- tierName(subgrid)[tierType(subgrid) != tier_type]

  type_label <- c(IntervalTier = "interval", TextTier = "text")[tier_type]

  if (length(wrong_type) == 1) {
    tier_name <- sprintf("'%s'", wrong_type)
    article <- ifelse(type_label == "interval", "an", "a")
    msg <- sprintf("The following tier is not %s %s tier in '%s': %s",
                   article, type_label, textgrid_name, tier_name)
  }

  if (1 < length(wrong_type)) {
    tier_name <- paste(sprintf("'%s'", wrong_type), collapse = ", ")
    msg <- sprintf("The following tiers are not %s tiers in '%s': %s",
                   type_label, textgrid_name, tier_name)
  }

  msg
}


#' Check that the named tiers are interval tiers
#' @inheritParams has_tiers
#' @return TRUE if the test passes
#' @export
is_interval_tier <- function(textgrid, tiers) {
  is_tier_type(textgrid, tiers, "IntervalTier")
}

# Check that a table is part of a db connection (error message)
on_failure(is_interval_tier) <- function(call, env) {
  # eval(call$tiers, env) looks up the value of 'tiers' in the environment of
  # the failed function call
  tiers <- eval(call$tiers, env)
  textgrid <- eval(call$textgrid, env)

  validate_that(is_tier_type(textgrid, tiers, "IntervalTier"))
}

#' Check that the named tiers are text/point tiers
#' @inheritParams has_tiers
#' @return TRUE if the test passes
#' @export
is_text_tier <- function(textgrid, tiers) {
  is_tier_type(textgrid, tiers, "TextTier")
}

# Check that a table is part of a db connection (error message)
on_failure(is_text_tier) <- function(call, env) {
  # eval(call$tiers, env) looks up the value of 'tiers' in the environment of
  # the failed function call
  tiers <- eval(call$tiers, env)
  textgrid <- eval(call$textgrid, env)

  validate_that(is_tier_type(textgrid, tiers, "TextTier"))
}
