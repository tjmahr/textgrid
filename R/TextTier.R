

## helpers --------------------------------------------------------------------

.GetPointTimes <- function(praatText) {
  # .GetPointTimesFromPraatText is a utility function for extracting the times of
  # the points in a TextTier from the text of a Praat TextGrid file that defines
  # the TextTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines a TextTier.
  #              See the documentation of TextTier.character for more information
  #              on how praatText is structured.
  #
  # Returns:
  #   A numeric vector comprising the times of the TextTier's points.

  # Set the regex pattern for the lines of praatText that contain the point
  # times of the TextTier.
  point.times.pattern <- '^ {12}(time|number)'
  # Grep the lines of praatText that contain the point times.
  praat.text.lines <- grep(point.times.pattern, praatText, value=TRUE)
  # Extract the point times from the lines of Praat text.
  point.times <- str_extract(praat.text.lines, '[0123456789]+\\.?[0123456789]*')
  # Type shift the point times to numeric.
  point.times <- as.numeric(point.times)

  # Return the point times.
  return(point.times)
}

.GetPointMarks <- function(praatText) {
  # .GetPointMarksFromPraatText is a utility function for extracting the text of
  # each point in a TextTier from the text of a Praat TextGrid file that defines
  # the TextTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines a TextTier.
  #              See the documentation of TextTier.character for more information
  #              on how praatText is structured.
  #
  # Returns:
  #   A character vector comprising the text for each of the TextTier's points.

  # Set the regex pattern for the lines of praatText that contain the point
  # marks of the TextTier.
  point.marks.pattern <- '^ {12}(text|mark)'
  # Grep the lines of praatText that contain the point marks.
  praat.text.lines <- grep(point.marks.pattern, praatText, value=TRUE)
  # Each point mark is enclosed in double-quotes ("). Extract the point marks
  # along with the enclosing double-quotes.
  point.marks.with.quotes <- str_extract(praat.text.lines, '".*"')
  # Extract each point mark from the double-quotes.  (Empty point marks are
  # returned as NA.)
  point.marks <- str_extract(point.marks.with.quotes, '[^"]+')

  # Return a character vector of the point marks.
  return(point.marks)
}

.GetPointTierNumber <- function(praatText) {
  # .GetPointTierNoFromPraatText is a utility function for extracting the tier
  # number of a TextTier from the text of a Praat TextGrid file that defines the
  # TextTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines a TextTier.
  #              See the documentation of TextTier.character for more information
  #              on how praatText is structured.
  #
  # Returns:
  #   An integer that specifies the tier number of the TextTier defined by
  #   praatText.

  # Set the regex pattern for the line of praatText that contains the tier
  # number.
  tier.number.pattern <- '^ {4}item'
  # Grep the line of praatText that contains the tier name.
  praat.text.line <- grep(tier.number.pattern, praatText, value=TRUE)
  # The tier number is enclosed in brackets ([).  Extract the tier number
  # along with the enclosing brackets.
  tier.number.with.brackets <- str_extract(praat.text.line, '\\[.*\\]')
  # Extract the tier number from the brackets.
  tier.number <- str_extract(tier.number.with.brackets, '[0123456789]+')
  # Type shift the tier number to a numeric.
  tier.number <- as.numeric(tier.number)

  # Return the tier number.
  return(tier.number)
}

.GetPointTierName <- function(praatText) {
  # .GetPointTierNameFromPraatText is a utility function for extracting the tier name
  # of a TextTier from the text of a Praat TextGrid file that defines the
  # TextTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines a TextTier.
  #              See the documentation of TextTier.character for more information
  #              on how praatText is structured.
  #
  # Returns:
  #   A character string that specifies the tier name of the TextTier defined by
  #   praatText.

  # Set the regex pattern for the line of praatText that contains the
  # tier name.
  tier.name.pattern <- '^ {8}name'
  # Grep the line of praatText that contains the tier name.
  praat.text.line <- grep(tier.name.pattern, praatText, value=TRUE)
  # The tier name is enclosed in double-quotes ("). Extract the tier name
  # along with the enclosing double-quotes.
  tier.name.with.quotes <- str_extract(praat.text.line, '".*"')
  # Extract the tier name from the double-quotes.
  tier.name <- str_extract(tier.name.with.quotes, '[^"]+')

  # Return the tier name.
  return(tier.name)
}

.GetPointTierXMin <- function(praatText) {
  # .GetPointTierXMinFromPraatText is a utility function for extracting the start
  # time of a TextTier from the text of a Praat TextGrid file that defines the
  # TextTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines a TextTier.
  #              See the documentation of TextTier.character for more information
  #              on how praatText is structured.
  #
  # Returns:
  #   A floating point number that specifies the start time of the TextTier
  #   defined by praatText.

  # Set the regex pattern for the line of praatText that contains the
  # start time.
  start.time.pattern <- '^ {8}xmin'
  # Grep the line of praatText that contains the start time.
  praat.text.line <- grep(start.time.pattern, praatText, value=TRUE)
  # Extract the start time from the line of Praat text.
  start.time <- str_extract(praat.text.line, '[0123456789]+\\.?[0123456789]*')
  # Type shift start.time to a numeric.
  start.time <- as.numeric(start.time)

  # Return the start time.
  return(start.time)
}

.GetPointTierXMax <- function(praatText) {
  # .GetPointTierXMaxFromPraatText is a utility function for extracting the end
  # time of a TextTier from the text of a Praat TextGrid file that defines the
  # TextTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines a TextTier.
  #              See the documentation of TextTier.character for more information
  #              on how praatText is structured.
  #
  # Returns:
  #   A floating point number that specifies the end time of the TextTier
  #   defined by praatText.

  # Set the regex pattern for the line of praatText that contains the
  # end time.
  end.time.pattern <- '^ {8}xmax'
  # Grep the line of praatText that contains the end time.
  praat.text.line <- grep(end.time.pattern, praatText, value=TRUE)
  # Extract the end time from the line of Praat text.
  end.time <- str_extract(praat.text.line, '[0123456789]+\\.?[0123456789]*')
  # Type shift end.time to a numeric.
  end.time <- as.numeric(end.time)

  # Return the end time.
  return(end.time)
}




## class definition -----------------------------------------------------------

setClass(Class          = 'TextTier',
         representation = representation(tierNumber = 'numeric',
                                         tierName   = 'character',
                                         size       = 'numeric',
                                         startTime  = 'numeric',
                                         endTime    = 'numeric',
                                         timeUnit   = 'character'),
         contains       = c('data.frame'),
         validity       = function(object) {
           valid.time.units <- c('seconds', 'milliseconds',
                                 'microseconds', 'nanoseconds')
           if (! timeUnit(object) %in% valid.time.units) {
             x <- paste('The timeUnit must be one of:',
                        'seconds, milliseconds, microseconds,',
                        'or nanoseconds')
             return(x)
           }
           TRUE
         }
)




## constructors ---------------------------------------------------------------

setGeneric(name = 'TextTier',
           def  = function(tierData, ...) {standardGeneric('TextTier')}
)

setMethod(f          = 'TextTier',
          signature  = c(tierData = 'character'),
          definition = function(tierData) {
            # Create a data.frame with two columns: the first, a
            # numeric vector containing the times of the points
            # in the TextTier; and the second, a character vector
            # containing the text of the points in the TextTier.
            times <- .GetPointTimes(tierData)
            marks <- .GetPointMarks(tierData)
            tier <- list(Time=times, Mark=marks)
            tier <- data.frame(tier, stringsAsFactors=FALSE)
            # Initialize the TextTier object, setting the slot
            # values for: tierNumber, tierName, size, startTime,
            # endTime, and timeUnit.
            new(Class      = 'TextTier',
                tier,
                tierNumber = .GetPointTierNumber(tierData),
                tierName   = .GetPointTierName(tierData),
                size       = nrow(tier),
                startTime  = .GetPointTierXMin(tierData),
                endTime    = .GetPointTierXMax(tierData),
                timeUnit   = 'seconds'
            )
          }
)

setMethod(f = 'TextTier',
          signature  = c(tierData = 'data.frame'),
          definition = function(tierData, tierNumber, tierName, startTime, endTime, timeUnit) {
            new(Class = 'TextTier',
                tierData,
                tierNumber = tierNumber,
                tierName   = tierName,
                size       = nrow(tierData),
                startTime  = startTime,
                endTime    = endTime,
                timeUnit   = timeUnit)
          }
)

setMethod(
  f = 'TextTier',
  signature  = c(tierData = 'missing'),
  definition = function(tierNumber, tierName, startTime, endTime, timeUnit) {
    tierData <- data.frame(Time = numeric(0), Mark = character(0),
                           stringsAsFactors = FALSE)
    new(Class = 'TextTier',
        tierData,
        tierNumber = tierNumber,
        tierName   = tierName,
        size       = nrow(tierData),
        startTime  = startTime,
        endTime    = endTime,
        timeUnit   = timeUnit)
  }
)




## coercion -------------------------------------------------------------------

# Coercion to a data.frame
setAs(from = 'TextTier', to   = 'data.frame',
  def  = function(from) {
    df <- data.frame(from@.Data, stringsAsFactors = FALSE)
    names(df) <- from@names
    df$TierName <- tierName(from)
    df$TierNumber <- tierNumber(from)
    df$TimeUnit <- timeUnit(from)
    df$TierType <- "Text"
    df <- df[c("TierNumber", "TierName", "TierType", "TimeUnit", from@names)]
    return(df)
  }
)

#' @export
as.data.frame.TextTier <- function(x, ...) {
  as(x, "data.frame")
}


## getters --------------------------------------------------------------------

setMethod(f          = 'tierNumber',
          signature  = c(.Object = 'TextTier'),
          definition = function(.Object) {.Object@tierNumber}
)

setMethod(f          = 'tierName',
          signature  = c(.Object = 'TextTier'),
          definition = function(.Object) {.Object@tierName}
)

setMethod(f          = 'size',
          signature  = c(.Object = 'TextTier'),
          definition = function(.Object) {.Object@size}
)

setMethod(f          = 'startTime',
          signature  = c(.Object = 'TextTier'),
          definition = function(.Object) {.Object@startTime}
)

setMethod(f          = 'endTime',
          signature  = c(.Object = 'TextTier'),
          definition = function(.Object) {.Object@endTime}
)

setMethod(f          = 'timeUnit',
          signature  = c(.Object = 'TextTier'),
          definition = function(.Object) {.Object@timeUnit}
)




## setters --------------------------------------------------------------------

setReplaceMethod(f          = 'tierNumber',
                 signature  = c(.Object = 'TextTier', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@tierNumber <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'tierName',
                 signature  = c(.Object = 'TextTier', value = 'character'),
                 definition = function(.Object, value) {
                   .Object@tierName <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'size',
                 signature  = c(.Object = 'TextTier', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@size <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'startTime',
                 signature  = c(.Object = 'TextTier', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@startTime <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'endTime',
                 signature  = c(.Object = 'TextTier', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@endTime <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'timeUnit',
                 signature  = c(.Object = 'TextTier', value = 'character'),
                 definition = function(.Object, value) {
                   # Get the current timeUnit of .Object, and
                   # convert it to a multiplicative factor.
                   old.unit <- timeUnit(.Object)
                   old.m <- .ConvertUnitNameToMultiplier(old.unit)
                   # Convert the new timeUnit to a multiplicative
                   # factor.
                   new.m <- .ConvertUnitNameToMultiplier(value)
                   # Create the conversion factor.
                   conv <- new.m / old.m
                   # Change the Time value of each point.
                   .Object$Time <- .Object$Time * conv
                   # Change the startTime and endTime of the tier.
                   startTime(.Object) <- startTime(.Object) * conv
                   endTime(.Object) <- endTime(.Object) * conv
                   # Change the timeUnit of the tier.
                   .Object@timeUnit <- value
                   return(.Object)
                 }
)




## primitive methods ----------------------------------------------------------

setMethod(f = '[',
          signature  = c(x = 'TextTier', i = 'ANY', j = 'ANY'),
          definition = function(x, i, j) {
            # Coerce the TextTier to a data.frame, and then slice
            # the data.frame.
            tier.data <- as(x, 'data.frame')
            tier.data <- tier.data[i, j]
            # Construct a new TextTier object from tier.data.
            new(Class = 'TextTier',
                tier.data,
                tierNumber = tierNumber(x),
                tierName   = tierName(x),
                size       = nrow(tier.data),
                startTime  = (tier.data$Time)[1],
                endTime    = (tier.data$Time)[nrow(tier.data)],
                timeUnit   = timeUnit(x)
            )
          }
)




## methods --------------------------------------------------------------------

setMethod(f = 'FormatAsPraatText',
          signature  = c(x = 'TextTier'),
          definition = function(x) {
            praat.text <- c(sprintf('    item [%d]:', tierNumber(x)),
                            '        class = "TextTier" ',
                            sprintf('        name = "%s" ', tierName(x)),
                            sprintf('        xmin = %.14f ', startTime(x)),
                            sprintf('        xmax = %.14f ', endTime(x)),
                            sprintf('        points: size = %d ', size(x)))
            for (int in 1:nrow(x)) {
              praat.text <- c(praat.text,
                              sprintf('        points [%d]:', int),
                              sprintf('            number = %.14f ', x$Time[int]),
                              sprintf('            mark = "%s" ', x$Mark[int]))
            }
            praat.text <- paste(praat.text, collapse = '\n')
            return(praat.text)
          }
)

setMethod(f          = 'TimeSlice',
          signature  = c(x = 'TextTier', sliceFrom = 'numeric',
                         sliceTo = 'numeric'),
          definition = function(x, sliceFrom, sliceTo, sliceUnit) {
            # Convert the sliceFrom and sliceTo values to the same
            # unit as the Time values of the TextTier object.
            tier.m <- .ConvertUnitNameToMultiplier(timeUnit(x))
            slice.m <- .ConvertUnitNameToMultiplier(sliceUnit)
            conv <- tier.m / slice.m
            slice.from <- sliceFrom * conv
            slice.to <- sliceTo * conv
            # Find the points in the TextTier whose Time value
            # falls after slice.from, but before slice.to.
            after.slice.from <- which(x$Time >= slice.from)
            before.slice.to <- which(x$Time <= slice.to)
            in.slice.interval <- intersect(after.slice.from,
                                           before.slice.to)
            # Slice the TextTier object.
            sliced.tier <- x[in.slice.interval, ]
            # Change the startTime and endTime of the sliced
            # TextTier.
            startTime(sliced.tier) <- slice.from
            endTime(sliced.tier) <- slice.to
            # Return the sliced TextTier object.
            return(sliced.tier)
          }
)
