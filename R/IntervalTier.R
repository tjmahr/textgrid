

## helpers --------------------------------------------------------------------

.GetIntervalXMins <- function(praatText) {
  # .GetIntervalXMinsFromPraatText is a utility function for extracting the start
  # time of each interval in an IntervalTier from the text of a Praat TextGrid
  # file that defines the IntervalTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines an IntervalTier.
  #              See the documentation of InvtervalTier.character for more
  #              information on how praatText is structured.
  #
  # Returns:
  #   A numeric vector comprising the start time of each of the IntervalTier's
  #   intervals.

  # Set the regex pattern for the lines of praatText that contain the interval
  # start times of the IntervalTier.
  interval.xmins.pattern <- '^ {12}xmin'
  # Grep the lines of praatText that contain the interval start times.
  praat.text.lines <- grep(interval.xmins.pattern, praatText, value=TRUE)
  # Extract the interval start times from the lines of Praat text.
  interval.xmins <- stringr::str_extract(praat.text.lines,
                                '[0123456789]+\\.?[0123456789]*')
  # Type shift the interval start times to numeric.
  interval.xmins <- as.numeric(interval.xmins)

  # Return the interval start times.
  return(interval.xmins)
}

.GetIntervalXMaxs <- function(praatText) {
  # .GetIntervalXMaxsFromPraatText is a utility function for extracting the end
  # time of each interval in an IntervalTier from the text of a Praat TextGrid
  # file that defines the IntervalTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines an IntervalTier.
  #              See the documentation of InvtervalTier.character for more
  #              information on how praatText is structured.
  #
  # Returns:
  #   A numeric vector comprising the end time of each of the IntervalTier's
  #   intervals.

  # Set the regex pattern for the lines of praatText that contain the interval
  # end times of the IntervalTier.
  interval.xmaxs.pattern <- '^ {12}xmax'
  # Grep the lines of praatText that contain the interval end times.
  praat.text.lines <- grep(interval.xmaxs.pattern, praatText, value=TRUE)
  # Extract the interval end times from the lines of Praat text.
  interval.xmaxs <- stringr::str_extract(praat.text.lines,
                                '[0123456789]+\\.?[0123456789]*')
  # Type shift the interval end times to numeric.
  interval.xmaxs <- as.numeric(interval.xmaxs)

  # Return the interval end times.
  return(interval.xmaxs)

}

.GetIntervalTexts <- function(praatText) {
  # .GetIntervalTextFromPraatText is a utility function for extracting the text
  # of each interval in an IntervalTier from the text of a Praat TextGrid file
  # that defines the IntervalTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines an IntervalTier.
  #              See the documentation of InvtervalTier.character for more
  #              information on how praatText is structured.
  #
  # Returns:
  #   A character vector comprising the text of each of the IntervalTier's
  #   intervals.

  # Set the regex pattern for the lines of praatText that contain the text
  # of the intervals in the IntervalTier.
  interval.texts.pattern <- '^ {12}(text|mark)'
  # Grep the lines of praatText that contain the text of the intervals.
  praat.text.lines <- grep(interval.texts.pattern, praatText, value=TRUE)
  # Each interval text is enclosed in double-quotes (").  Extract the interval
  # texts along with the enclosing double-quotes.
  interval.texts.with.quotes <- stringr::str_extract(praat.text.lines, '".*"')
  # Extract each interval text from the double-quotes.  (Empty intervals are
  # returned as NA.)
  interval.texts <- stringr::str_extract(interval.texts.with.quotes, '[^"]+')

  # Return a character vector of the interval texts.
  return(interval.texts)
}

.GetIntervalTierNumber <- function(praatText) {
  # .GetIntervalTierNoFromPraatText is a utility function for extracting the tier
  # number of an IntervalTier from the text of a Praat TextGrid file that defines
  # the IntervalTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines an IntervalTier.
  #              See the documentation of InvtervalTier.character for more
  #              information on how praatText is structured.
  #
  # Returns:
  #   An integer that specifies the tier number of the IntervalTier defined by
  #   praatText.

  # Set the regex pattern for the line of praatText that contains the tier
  # number.
  tier.number.pattern <- '^ {4}item'
  # Grep the line of praatText that contains the tier name.
  praat.text.line <- grep(tier.number.pattern, praatText, value=TRUE)
  # The tier number is enclosed in brackets ([).  Extract the tier number
  # along with the enclosing brackets.
  tier.number.with.brackets <- stringr::str_extract(praat.text.line, '\\[.*\\]')
  # Extract the tier number from the brackets.
  tier.number <- stringr::str_extract(tier.number.with.brackets, '[0123456789]+')
  # Type shift the tier number to a numeric.
  tier.number <- as.numeric(tier.number)

  # Return the tier number.
  return(tier.number)
}

.GetIntervalTierName <- function(praatText) {
  # .GetIntervalTierNameFromPraatText is a utility function for extracting the
  # tier name of a IntervalTier from the text of a Praat TextGrid file that
  # defines the IntervalTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines a IntervalTier.
  #              See the documentation of IntervalTier.character for more
  #              information on how praatText is structured.
  #
  # Returns:
  #   A character string that specifies the tier name of the IntervalTier defined by
  #   praatText.

  # Set the regex pattern for the line of praatText that contains the
  # tier name.
  tier.name.pattern <- '^ {8}name'
  # Grep the line of praatText that contains the tier name.
  praat.text.line <- grep(tier.name.pattern, praatText, value=TRUE)
  # The tier name is enclosed in double-quotes ("). Extract the tier name
  # along with the enclosing double-quotes.
  tier.name.with.quotes <- stringr::str_extract(praat.text.line, '".*"')
  # Extract the tier name from the double-quotes.
  tier.name <- stringr::str_extract(tier.name.with.quotes, '[^"]+')

  # Return the tier name.
  return(tier.name)
}

.GetIntervalTierXMin <- function(praatText) {
  # .GetIntervalTierXMinFromPraatText is a utility function for extracting the
  # start time of a IntervalTier from the text of a Praat TextGrid file that
  # defines the IntervalTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines a TextTier.
  #              See the documentation of IntervalTier.character for more
  #              information on how praatText is structured.
  #
  # Returns:
  #   A floating point number that specifies the start time of the IntervalTier
  #   defined by praatText.

  # Set the regex pattern for the line of praatText that contains the
  # start time.
  start.time.pattern <- '^ {8}xmin'
  # Grep the line of praatText that contains the start time.
  praat.text.line <- grep(start.time.pattern, praatText, value=TRUE)
  # Extract the start time from the line of Praat text.
  start.time <- stringr::str_extract(praat.text.line, '[0123456789]+\\.?[0123456789]*')
  # Type shift start.time to a numeric.
  start.time <- as.numeric(start.time)

  # Return the start time.
  return(start.time)
}

.GetIntervalTierXMax <- function(praatText) {
  # .GetIntervalTierXMaxFromPraatText is a utility function for extracting the end
  # time of a IntervalTier from the text of a Praat TextGrid file that defines the
  # IntervalTier.
  # Arguments:
  #   praatText: A character vector, each element of which is a line from the
  #              part of a Praat TextGrid file that defines a IntervalTier.
  #              See the documentation of IntervalTier.character for more
  #              information on how praatText is structured.
  #
  # Returns:
  #   A floating point number that specifies the end time of the IntervalTier
  #   defined by praatText.

  # Set the regex pattern for the line of praatText that contains the
  # end time.
  end.time.pattern <- '^ {8}xmax'
  # Grep the line of praatText that contains the end time.
  praat.text.line <- grep(end.time.pattern, praatText, value=TRUE)
  # Extract the end time from the line of Praat text.
  end.time <- stringr::str_extract(praat.text.line, '[0123456789]+\\.?[0123456789]*')
  # Type shift end.time to a numeric.
  end.time <- as.numeric(end.time)

  # Return the end time.
  return(end.time)
}




## class definition -----------------------------------------------------------

setClass(Class          = 'IntervalTier',
         representation = representation(tierNumber = 'numeric',
                                         tierName   = 'character',
                                         size       = 'numeric',
                                         startTime  = 'numeric',
                                         endTime    = 'numeric',
                                         timeUnit   = 'character'),
         contains       = c('data.frame'),
         validity       = function(object) {
           if (! identical(names(object), c('XMin', 'XMax', 'Text'))) {
             x <- paste('The column names of the data.frame',
                        'that defines the intervals must be:',
                        'XMin, XMax, and Text')
             return(x)
           }
           if (object$XMin[1] != startTime(object)) {
             x <- paste('The startTime of the first interval',
                        'and the startTime of the',
                        'IntervalTier do not match.')
             return(x)
           }
           if (object$XMax[size(object)] != endTime(object)) {
             x <- paste('The endTime of the last interval',
                        'and the endTime of the',
                        'IntervalTier do not match.')
             return(x)
           }
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

setGeneric(name = 'IntervalTier',
           def  = function(tierData, ...) {standardGeneric('IntervalTier')}
)

setMethod(f          = 'IntervalTier',
          signature  = c(tierData = 'character'),
          definition = function(tierData) {
            # Create a data.frame with three columns: the first,
            # a numeric vector containing the start times of the
            # intervals in the IntervalTier; the second, a numeric
            # vector containing the end times; and the third, a
            # character vector containing the intervals' text.
            xmins <- .GetIntervalXMins(tierData)
            xmaxs <- .GetIntervalXMaxs(tierData)
            texts <- .GetIntervalTexts(tierData)
            tier <- list(XMin = xmins, XMax = xmaxs, Text = texts)
            tier <- data.frame(tier, stringsAsFactors=FALSE)
            # Initialize the IntervalTier object, setting the slot
            # values for: tierNumber, tierName, size, startTime,
            # endTime, and timeUnit.
            new(Class      = 'IntervalTier',
                tier,
                tierNumber = .GetIntervalTierNumber(tierData),
                tierName   = .GetIntervalTierName(tierData),
                size       = nrow(tier),
                startTime  = .GetIntervalTierXMin(tierData),
                endTime    = .GetIntervalTierXMax(tierData),
                timeUnit   = 'seconds'
            )
          }
)

setMethod(f = 'IntervalTier',
          signature  = c(tierData = 'data.frame'),
          definition = function(tierData, tierNumber, tierName, timeUnit) {
            if (nrow(tierData) == 0) {
              message('An IntervalTier object cannot be initialized from')
              message('a data.frame that has 0 rows.')
              return()
            }
            new(Class = 'IntervalTier',
                tierData,
                tierNumber = tierNumber,
                tierName   = tierName,
                size       = nrow(tierData),
                startTime  = tierData$XMin[1],
                endTime    = tierData$XMax[nrow(tierData)],
                timeUnit   = timeUnit)
          }
)

setMethod(f = 'IntervalTier',
          signature = c(tierData = 'missing'),
          definition = function(tierNumber, tierName, startTime, endTime, timeUnit) {
            new(Class = 'IntervalTier',
                data.frame(XMin = startTime, XMax = endTime,
                           Text = NA_character_, stringsAsFactors = FALSE),
                tierNumber = tierNumber,
                tierName   = tierName,
                size       = 1,
                startTime  = startTime,
                endTime    = endTime,
                timeUnit   = timeUnit)
          }
)




## coercion -------------------------------------------------------------------

# Coercion to a data.frame
setAs(from = 'IntervalTier', to   = 'data.frame',
  def  = function(from) {
    df <- data.frame(from@.Data, stringsAsFactors = FALSE)
    names(df) <- from@names
    df$TierName <- tierName(from)
    df$TierNumber <- tierNumber(from)
    df$TierType <- "Interval"
    df$TimeUnit <- timeUnit(from)
    df <- df[c("TierNumber", "TierName", "TierType", "TimeUnit", from@names)]
    return(df)
  }
)

#' @export
as.data.frame.IntervalTier <- function(x, ...) {
  as(x, "data.frame")
}


## getters --------------------------------------------------------------------

setMethod(f          = 'tierNumber',
          signature  = c(.Object = 'IntervalTier'),
          definition = function(.Object) {.Object@tierNumber}
)

setMethod(f          = 'tierName',
          signature  = c(.Object = 'IntervalTier'),
          definition = function(.Object) {.Object@tierName}
)

setMethod(f          = 'size',
          signature  = c(.Object = 'IntervalTier'),
          definition = function(.Object) {.Object@size}
)

setMethod(f          = 'startTime',
          signature  = c(.Object = 'IntervalTier'),
          definition = function(.Object) {.Object@startTime}
)

setMethod(f          = 'endTime',
          signature  = c(.Object = 'IntervalTier'),
          definition = function(.Object) {.Object@endTime}
)

setMethod(f          = 'timeUnit',
          signature  = c(.Object = 'IntervalTier'),
          definition = function(.Object) {.Object@timeUnit}
)




## setters --------------------------------------------------------------------

setReplaceMethod(f          = 'tierNumber',
                 signature  = c(.Object = 'IntervalTier', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@tierNumber <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'tierName',
                 signature  = c(.Object = 'IntervalTier', value = 'character'),
                 definition = function(.Object, value) {
                   .Object@tierName <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'size',
                 signature  = c(.Object = 'IntervalTier', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@size <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'startTime',
                 signature  = c(.Object = 'IntervalTier', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@startTime <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'endTime',
                 signature  = c(.Object = 'IntervalTier', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@endTime <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'timeUnit',
                 signature  = c(.Object = 'IntervalTier', value = 'character'),
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
                   # Change the XMin and XMax value of each
                   # interval.
                   .Object$XMin <- .Object$XMin * conv
                   .Object$XMax <- .Object$XMax * conv
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
          signature  = c(x = 'IntervalTier', i = 'ANY', j = 'ANY'),
          definition = function(x, i, j) {
            # Coerce the IntervalTier to a data.frame, and then
            # slice the data.frame.
            tier.data <- as(x, 'data.frame')
            tier.data <- tier.data[i, j]
            # Construct a new IntervalTier object from tier.data.
            new(Class      = 'IntervalTier',
                tier.data,
                tierNumber = tierNumber(x),
                tierName   = tierName(x),
                size       = nrow(tier.data),
                startTime  = (tier.data$XMin)[1],
                endTime    = (tier.data$XMax)[nrow(tier.data)],
                timeUnit   = timeUnit(x)
            )
          }
)




## methods --------------------------------------------------------------------

setMethod(f = 'FormatAsPraatText',
          signature  = c(x = 'IntervalTier'),
          definition = function(x) {
            # Convert all <NA> interval labels to '' labels.
            x$Text[is.na(x$Text)] <- ''
            praat.text <- c(sprintf('    item [%d]:', tierNumber(x)),
                            '        class = "IntervalTier" ',
                            sprintf('        name = "%s" ', tierName(x)),
                            sprintf('        xmin = %.14f ', startTime(x)),
                            sprintf('        xmax = %.14f ', endTime(x)),
                            sprintf('        intervals: size = %d ', size(x)))
            for (int in 1:nrow(x)) {
              praat.text <- c(praat.text,
                              sprintf('        intervals [%d]:', int),
                              sprintf('            xmin = %.14f ', x$XMin[int]),
                              sprintf('            xmax = %.14f ', x$XMax[int]),
                              sprintf('            text = "%s" ', x$Text[int]))
            }
            praat.text <- paste(praat.text, collapse = '\n')
            return(praat.text)
          }
)

setMethod(f          = 'TimeSlice',
          signature  = c(x = 'IntervalTier', sliceFrom = 'numeric',
                         sliceTo = 'numeric'),
          definition = function(x, sliceFrom, sliceTo, sliceUnit) {
            # Convert the sliceFrom and sliceTo values to the same
            # unit as the Time values of the IntervalTier object.
            tier.m <- .ConvertUnitNameToMultiplier(timeUnit(x))
            slice.m <- .ConvertUnitNameToMultiplier(sliceUnit)
            conv <- tier.m / slice.m
            slice.from <- sliceFrom * conv
            slice.to <- sliceTo * conv
            # Find the interval that contains the slice.from time.
            # If the slice.from time occurs at an interval
            # boundary, then the later interval is included, but
            # the earlier interval is not.
            diffs <- slice.from - x$XMin
            slice.from.index <- length(which(diffs >= 0))
            # Find the interval that contains the slice.to time.
            # If the slice.to time occurs at an interval boundary,
            # then the earlier interval is included, but the later
            # interval is not.
            diffs <- slice.to - x$XMin
            slice.to.index <- length(which(diffs > 0))
            # Slice the IntervalTier as a data.frame.
            sliced.tier <- x[slice.from.index:slice.to.index, ]
            # Change the XMin value of the first interval in the
            # sliced IntervalTier.
            sliced.tier$XMin[1] <- slice.from
            # Change the XMax value of the last interval in the
            # sliced IntervalTier.
            sliced.tier$XMax[nrow(sliced.tier)] <- slice.to
            # Change the startTime and endTime slot-values of the
            # sliced IntervalTier.
            startTime(sliced.tier) <- slice.from
            endTime(sliced.tier) <- slice.to
            # Return the sliced IntervalTier.
            return(sliced.tier)
          }
)
