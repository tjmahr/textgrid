#' textgrid: S4 classes for Praat Text-Grids
#' @docType package
#' @name textgrid
#' @rdname textgrid-package
#' @import stringr assertthat
NULL



# Package-level constants
patterns <- list(
  tier = '^ {4}item',
  tg_min_time = '^xmin',
  tg_max_time = '^xmax',
  class = '^ {8}class',
  tier_name = '^ {8}name'
)
