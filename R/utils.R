
.ConvertUnitNameToMultiplier <- function(unitName) {
  # .ConvertUnitNameToMultiplier is a utility function for converting the
  # time unit c('second', 'millisecond', 'microsecond', 'nanosecond') to
  # proportions of one second.
  # .ConverUnitNameToMultiplier implements the following map:
  #        'seconds'   -->   1
  #   'milliseconds'   -->   1000
  #   'microseconds'   -->   1000000
  #    'nanoseconds'   -->   1000000000
  if (unitName == 'seconds') {
    multiplier <- 1
  } else if (unitName == 'milliseconds') {
    multiplier <- 1000
  } else if (unitName == 'microseconds') {
    multiplier <- 1000000
  } else if (unitName == 'nanoseconds') {
    multiplier <- 1000000000
  }
  return(multiplier)
}
