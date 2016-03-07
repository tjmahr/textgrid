

## helpers --------------------------------------------------------------------

.FindTierStartsInPraatText <- function(praatTGridText) {
  # .FindTierStartsInPraatText is a utility function for finding the line numbers
  # of a TextGrid file where the tiers of that TextGrid begin.
  # Arguments:
  #   praatTGridText: A character vector, each element of which is a line of
  #                   a Praat TextGrid file.
  #
  # Returns:
  #   A numeric vector.  Each element of the numeric vector is the line number
  #   of the TextGrid file where a tier of the TextGrid begins.

  # Set the regex pattern for the start of a tier.
  start.of.tier.pattern <- '^ {4}item'
  # Grep the lines of praatTGridText that match start.of.tier.pattern.
  line.numbers.of.matches <- grep(start.of.tier.pattern, praatTGridText)

  # Return the line numbers of the matches.
  return(line.numbers.of.matches)
}

.PortionPraatTextByTier <- function(praatTGridText) {
  # .PortionPraatTextByTier is a utility function for separating the lines of
  # text of a Praat TextGrid into the text for each tier.
  # Arguments:
  #   praatTGridText: A character vector, each element of which is a line of
  #                   a Praat TextGrid file.
  #
  # Returns:
  #   A list.  Each element of the list is a character vector comprising the
  #   lines of praatTGridText that define either a TextTier or an IntervalTier.

  # Find the beginning of each tier.
  tier.start.indices <- .FindTierStartsInPraatText(praatTGridText)

  # Make a numeric vector of the ending indices of each tier.
  number.of.tiers <- length(tier.start.indices)
  if (length(tier.start.indices) > 1)
    tier.end.indices <- c(tier.start.indices[2:number.of.tiers] - 1,
                          length(praatTGridText))
  else
    tier.end.indices <- length(praatTGridText)

  # Make a list, each element of which is a numeric vector comprising the
  # indices of praatTGridText occupied by each tier.
  tier.indices <- Map(`:`, tier.start.indices, tier.end.indices)

  # Loop through tier.indices, slicing praatTGridText.
  praat.text.into.tiers <- list()
  for (indices in tier.indices) {
    extracted.tier <- list(praatTGridText[indices])
    praat.text.into.tiers <- c(praat.text.into.tiers, extracted.tier)
  }

  # Return the TextGrid text separated into tiers
  return(praat.text.into.tiers)
}

.GetTierClassFromPraatText <- function(praatTierText) {
  # .GetTierClassFromPraatText is a utility function for extracting the class
  # of a tier from the text of a Praat TextGrid file that defines the tier.
  # Arguments:
  #   praatTierText: A character vector, each element of which is a line from the
  #                  part of a Praat TextGrid file that defines a tier.
  #
  # Returns:
  #   A character string that specifies the class of the tier defined by
  #   praatTierText.

  # Set the regex pattern for the line of praatTierText that contains the
  # class of the tier.
  tier.class.pattern <- '^ {8}class'
  # Grep the line of praatTierText that contains the class of the tier.
  praat.text.line <- grep(tier.class.pattern, praatTierText, value=TRUE)
  # The class of the tier is enclosed in double-quotes (").  Extract the class
  # of the tier along with the enclosing double-quotes.
  tier.class.with.quotes <- stringr::str_extract(praat.text.line, '".*"')
  # Extract the tier class from the double-quotes.
  tier.class <- stringr::str_extract(tier.class.with.quotes, '[^"]+')

  # Return the tier class.
  return(tier.class)
}

.GetTierNameFromPraatText <- function(praatTierText) {
  # .GetTierNameFromPraatText is a utility function for extracting the name
  # of a tier from the text of a Praat TextGrid file that defines the tier.
  # Arguments:
  #   praatTierText: A character vector, each element of which is a line from the
  #                  part of a Praat TextGrid file that defines a tier.
  #
  # Returns:
  #   A character string that specifies the name of the tier defined by
  #   praatTierText.

  # Set the regex pattern for the line of praatTierText that contains the
  # name of the tier.
  tier.name.pattern <- '^ {8}name'
  # Grep the line of praatTierText that contains the name of the tier.
  praat.text.line <- grep(tier.name.pattern, praatTierText, value=TRUE)
  # The name of the tier is enclosed in double-quotes (").  Extract the name
  # of the tier along with the enclosing double-quotes.
  tier.name.with.quotes <- stringr::str_extract(praat.text.line, '".*"')
  # Extract the tier name from the double-quotes.
  tier.name <- stringr::str_extract(tier.name.with.quotes, '[^"]+')

  # Return the tier name.
  return(tier.name)
}

.PraatTextToTierObject <- function(praatTierText) {
  # .PraatTextToTierObject is a utility function for constructing an IntervalTier
  # or TextTier object from Praat text that defines either an IntervalTier or
  # a TextTier, respectively.
  # Arguments:
  #   praatTierText: A character vector, each element of which is a line from the
  #                  part of a Praat TextGrid file that defines a tier.
  #
  # Returns:
  #   An IntervalTier or a TextTier object.

  # Get the class of the tier defined by praatTierText.
  tier.class <- .GetTierClassFromPraatText(praatTierText)

  # If the tier is an IntervalTier, create an IntervalTier object...
  if (tier.class == 'IntervalTier') {
    tier <- IntervalTier(praatTierText)
    # If the tier is a TextTier, create a TextTier object...
  } else if (tier.class == 'TextTier') {
    tier <- TextTier(praatTierText)
    # Otherwise, the tier is NULL.
  } else {
    tier <- NULL
  }

  # Return the tier object.
  return(tier)
}

.GetTGridXMinFromPraatText <- function(praatTGridText) {
  # .GetTGridXMinFromPraatText is a utility function for extracting the start
  # time of a TextGrid from the text of Praat TextGrid file.
  # Arguments:
  #   praatTGridText: A character vector, each element of which is a line of
  #                   a Praat TextGrid file.
  #
  # Returns:
  #   A floating point number, the start time of the TextGrid defined by
  #   praatTGridText.

  # Set the regex pattern for the line of praatTGridText that contains the
  # start time.
  start.time.pattern <- '^xmin'
  # Grep the line of praatTGridText that contains the start time.
  praat.text.line <- grep(start.time.pattern, praatTGridText, value=TRUE)
  # Extract the start time from the line of Praat text.
  start.time <- stringr::str_extract(praat.text.line, '[0123456789]+\\.?[0123456789]*')
  # Type shift start.time to a numeric.
  start.time <- as.numeric(start.time)

  # Return the start time.
  return(start.time)
}

.GetTGridXMaxFromPraatText <- function(praatTGridText) {
  # .GetTGridXMaxFromPraatText is a utility function for extracting the end
  # time of a TextGrid from the text of Praat TextGrid file.
  # Arguments:
  #   praatTGridText: A character vector, each element of which is a line of
  #                   a Praat TextGrid file.
  #
  # Returns:
  #   A floating point number, the end time of the TextGrid defined by
  #   praatTGridText.

  # Set the regex pattern for the line of praatTGridText that contains the
  # end time.
  end.time.pattern <- '^xmax'
  # Grep the line of praatTGridText that contains the end time.
  praat.text.line <- grep(end.time.pattern, praatTGridText, value=TRUE)
  # Extract the end time from the line of Praat text.
  end.time <- stringr::str_extract(praat.text.line, '[0123456789]+\\.?[0123456789]*')
  # Type shift end.time to a numeric.
  end.time <- as.numeric(end.time)

  # Return the end time.
  return(end.time)
}




## class definition -----------------------------------------------------------

validate_grid <- function(object) {
  # Check that the startTime of the TextGrid matches
  # the startTime of each tier.
  for (i in seq(size(object))) {
    misaligned.tiers <- c()
    if (startTime(object) != startTime(object[[i]])) {
      misaligned.tiers <- c(misaligned.tiers, i)
    }
  }
  if (length(misaligned.tiers) > 0) {
    tiers <- paste(misaligned.tiers, collapse = ', ')
    x <- paste('The startTime of the TextGrid does',
               'not match the startTime of Tier(s):',
               tiers)
    return(x)
  }
  # Check that the endTime of the TextGrid matches
  # the endTime of each tier.
  for (i in seq(size(object))) {
    misaligned.tiers <- c()
    if (endTime(object) != endTime(object[[i]])) {
      misaligned.tiers <- c(misaligned.tiers, i)
    }
  }
  if (length(misaligned.tiers) > 0) {
    tiers <- paste(misaligned.tiers, collapse = ', ')
    x <- paste('The endTime of the TextGrid does',
               'not match the endTime of Tier(s):',
               tiers)
    return(x)
  }
  # Check the timeUnit of the TextGrid.
  valid.time.units <- c('seconds', 'milliseconds',
                        'microseconds', 'nanoseconds')
  if (! timeUnit(object) %in% valid.time.units) {
    x <- paste('The timeUnit must be one of:',
               'seconds, milliseconds, microseconds,',
               'or nanoseconds')
    return(x)
  }
  # Check that the timeUnit of the TextGrid matches
  # the timeUnit of each tier.
  for (i in seq(size(object))) {
    mismatched.tiers <- c()
    if (timeUnit(object) != timeUnit(object[[i]])) {
      mismatched.tiers <- c(mismatched.tiers, i)
    }
  }
  if (length(mismatched.tiers) > 0) {
    tiers <- paste(mismatched.tiers, collapse = ', ')
    x <- paste('The timeUnit of the TextGrid does',
               'not match the timeUnit of Tier(s):',
               tiers)
    return(x)
  }
  TRUE
}



#' TextGrid class
#' @slot textGridName name of the text grid file
#' @slot size number of tiers
#' @slot startTime start time of text grid
#' @slot endTime end time of text grid
#' @slot timeUnit measurement unit of the start and end times (seconds,
#'   milliseconds, microseconds, nanoseconds)
#' @export TextGrid
#' @exportClass TextGrid
TextGrid <- setClass(
  Class = 'TextGrid',
  slots = list(textGridName = 'character',
               size = 'numeric',
               startTime = 'numeric',
               endTime = 'numeric',
               timeUnit = 'character'),
  contains = c('list'),
  validity = validate_grid
)




## constructors ---------------------------------------------------------------

#' @export
#' @rdname TextGrid
setGeneric(name = 'TextGrid',
           def  = function(x, ...) {standardGeneric('TextGrid')}
)

#' @rdname TextGrid
setMethod(f          = 'TextGrid',
          signature  = c(x = 'character'),
          definition = function(x) {
            # Read in the TextGrid line-by-line.
            tgrid.lines <- readLines(x)
            # Separate tgrid.lines into the lines that constitute
            # each tier.
            tier.lines.list <- .PortionPraatTextByTier(tgrid.lines)
            # Create tier objects from the Praat text lines that
            # define them.
            tier.objects.list <- Map(.PraatTextToTierObject, tier.lines.list)
            # Names for the tier objects on the list of tier
            # objects.
            tier.names <- Map(.GetTierNameFromPraatText, tier.lines.list)
            tier.names <- Reduce(c, tier.names)
            # Create a named list of the tier objects.
            text.grid <- tier.objects.list
            names(text.grid) <- tier.names
            new(Class = 'TextGrid',
                text.grid,
                textGridName = basename(x),
                size         = length(text.grid),
                startTime    = .GetTGridXMinFromPraatText(tgrid.lines),
                endTime      = .GetTGridXMaxFromPraatText(tgrid.lines),
                timeUnit     = 'seconds'
            )
          }
)

#' @rdname TextGrid
setMethod(f = 'TextGrid',
          signature  = c(x = 'IntervalTier'),
          definition = function(x, ...) {
            # Group the other tiers into a list
            other.tiers <- list(...)
            # Initialize a TextGrid-like list from the IntervalTier x.
            tierNumber(x)    <- 1
            text.grid        <- list(x)
            names(text.grid) <- tierName(x)
            for (tier in seq(along = other.tiers)) {
              next.tier              <- other.tiers[[tier]]
              tierNumber(next.tier)  <- tier + 1
              timeUnit(next.tier)    <- timeUnit(x)
              text.grid              <- c(text.grid, list(next.tier))
              names(text.grid)[tier + 1] <- tierName(next.tier)
            }
            new(Class = 'TextGrid',
                text.grid,
                textGridName = 'myTextGrid',
                size         = length(text.grid),
                startTime    = startTime(x),
                endTime      = endTime(x),
                timeUnit     = timeUnit(x))
          }
)




## coercion -------------------------------------------------------------------

setAs(from = 'TextGrid',
      to   = 'list',
      def  = function(from) {
        to.list <- from@.Data
        names(to.list) <- from@names
        return(to.list)
      }
)

setAs(from = 'TextGrid', to = 'data.frame',
  def = function(from) {
    # convert each tier to a data.frame and combine
    data <- from@.Data
    dfs <- lapply(data, data.frame)
    df <- dplyr::bind_rows(dfs)
    # include filename as column
    old_names <- names(df)
    df$TextGrid <- textGridName(from)
    df <- df[c("TextGrid", old_names)]
    # remove dplyr classes
    as.data.frame(df, stringsAsFactors = FALSE)
  }
)

#' @export
as.list.TextGrid <- function(x, ...) {
  as(x, "list")
}

#' @export
as.data.frame.TextGrid <- function(x, ...) {
  as(x, "data.frame")
}



## getters --------------------------------------------------------------------

#' @rdname TextGrid
setMethod(f          = 'textGridName',
          signature  = c(.Object = 'TextGrid'),
          definition = function(.Object) {.Object@textGridName}
)

#' @rdname TextGrid
setMethod(f          = 'size',
          signature  = c(.Object = 'TextGrid'),
          definition = function(.Object) {.Object@size}
)

#' @rdname TextGrid
setMethod(f          = 'startTime',
          signature  = c(.Object = 'TextGrid'),
          definition = function(.Object) {.Object@startTime}
)

#' @rdname TextGrid
setMethod(f          = 'endTime',
          signature  = c(.Object = 'TextGrid'),
          definition = function(.Object) {.Object@endTime}
)

#' @rdname TextGrid
setMethod(f          = 'timeUnit',
          signature  = c(.Object = 'TextGrid'),
          definition = function(.Object) {.Object@timeUnit}
)


## setters --------------------------------------------------------------------

setReplaceMethod(f          = 'textGridName',
                 signature  = c(.Object = 'TextGrid', value = 'character'),
                 definition = function(.Object, value) {
                   .Object@textGridName <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'size',
                 signature  = c(.Object = 'TextGrid', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@size <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'startTime',
                 signature  = c(.Object = 'TextGrid', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@startTime <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'endTime',
                 signature  = c(.Object = 'TextGrid', value = 'numeric'),
                 definition = function(.Object, value) {
                   .Object@endTime <- value
                   return(.Object)
                 }
)

setReplaceMethod(f          = 'timeUnit',
                 signature  = c(.Object = 'TextGrid', value = 'character'),
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
                   # Change the timeUnit of each tier in the
                   # TextGrid.
                   for (i in seq(size(.Object))) {
                     timeUnit(.Object[[i]]) <- value
                   }
                   # Change the startTime and endTime of the
                   # TextGrid.
                   startTime(.Object) <- startTime(.Object) * conv
                   endTime(.Object) <- endTime(.Object) * conv
                   # Change the timeUnit of the TextGrid.
                   .Object@timeUnit <- value
                   return(.Object)
                 }
)




## primitive methods ----------------------------------------------------------

setMethod(f          = '[',
          signature  = c(x = 'TextGrid', i = 'ANY'),
          definition = function(x, i) {
            # Coerce the TextGrid x to a list, and then index the list.
            x.list <- as(x, 'list')
            x.list <- x.list[i]
            # Construct a new TextGrid object from x.list.
            new(Class = 'TextGrid',
                x.list,
                textGridName = textGridName(x),
                size         = length(x.list),
                startTime    = startTime(x),
                endTime      = endTime(x),
                timeUnit     = timeUnit(x)
            )
          }
)

setMethod(f          = 'c',
          signature  = c(x = 'TextGrid'),
          definition = function(x, ...) {
            # Coerce the TextGrid x to a list.
            x.list <- as(x, 'list')
            # Group the other arguments ... into a list.
            other.args <- list(...)
            # Coerce each other argument to a list and accumulatively
            # concatenate it to x.list.
            for (other.arg in other.args) {
              timeUnit(other.arg) <- timeUnit(x)
              x.list <- c(x.list, as(other.arg, 'list'))
            }
            # Create a new TextGrid object from x.list.
            new(Class = 'TextGrid',
                x.list,
                textGridName = textGridName(x),
                size         = length(x.list),
                startTime    = startTime(x),
                endTime      = endTime(x),
                timeUnit     = timeUnit(x)
            )
          }
)




## methods --------------------------------------------------------------------

setMethod(show, signature(object = "TextGrid"),
  definition = function(object) {
    l1 <- paste0("Textgrid: ", textGridName(object))
    l2 <- paste0("Start time: ", startTime(object), " (", timeUnit(object), ")")
    l3 <- paste0("End time: ", endTime(object), " (", timeUnit(object), ")")
    l4 <- paste0("Tiers: ", size(object))

    tier_line <- function(tier, indent = "  ") {
      type <- stringr::str_replace(class(tier), "Tier", "")
      char <- if (size(tier) == 1) "" else "s"
      type <- tolower(paste0(type, char))
      t1 <- paste0(indent,
                   tierNumber(tier), ": ",
                   class(tier), " \'",  tierName(tier),
                   "\' with ", size(tier), " ", type)
      t1
    }

    tiers <- unlist(lapply(object@.Data, tier_line))
    tiers <- paste0(tiers, collapse = "\n")
    all <- paste(l1, l2, l3, l4, tiers, sep = "\n")
    cat(all)
  }
)



#' @rdname TextGrid
setMethod(f = 'tierName', signature  = c(object = 'TextGrid'),
  definition = function(object) {
    unlist(lapply(object@.Data, tierName))
  }
)


#' @rdname TextGrid
setMethod(f = 'FormatAsPraatText',
          signature  = c(x = 'TextGrid'),
          definition = function(x) {
            praat.text <- c('File type = "ooTextFile"',
                            'Object class = "TextGrid"',
                            '',
                            sprintf('xmin = %.14f ', startTime(x)),
                            sprintf('xmax = %.14f ', endTime(x)),
                            'tiers? <exists> ',
                            sprintf('size = %d ', size(x)),
                            'item []: ')
            for (tier in x) {
              praat.text <- c(praat.text,
                              FormatAsPraatText(tier))
            }
            praat.text <- paste(praat.text, collapse = '\n')
            return(praat.text)
          }
)

#' @rdname TextGrid
setMethod(f          = 'TimeSlice',
          signature  = c(x = 'TextGrid', sliceFrom = 'numeric',
                         sliceTo = 'numeric'),
          definition = function(x, sliceFrom, sliceTo, sliceUnit) {
            # Coerce the TextGrid to a list.
            x.list <- as(x, 'list')
            # TimeSlice each tier on the TextGrid list.
            sliced.tgrid <- lapply(x.list, TimeSlice,
                                   sliceFrom = sliceFrom,
                                   sliceTo   = sliceTo,
                                   sliceUnit = sliceUnit)
            # Create a new TextGrid object.
            new(Class        = 'TextGrid',
                sliced.tgrid,
                textGridName = textGridName(x),
                size         = length(sliced.tgrid),
                startTime    = startTime(sliced.tgrid[[1]]),
                endTime      = endTime(sliced.tgrid[[1]]),
                timeUnit     = timeUnit(x)
            )
          }
)


#' @rdname TextGrid
setMethod(f          = 'GetIntervalText',
          signature  = c(textGrid = 'TextGrid', tier = 'ANY', interval = 'numeric'),
          definition = function(textGrid, tier, interval) {
            textGrid[[tier]]$Text[interval]
          }
)

#' @rdname TextGrid
setMethod(f          = 'GetIntervalXMin',
          signature  = c(textGrid = 'TextGrid', tier = 'ANY', interval = 'numeric'),
          definition = function(textGrid, tier, interval) {
            textGrid[[tier]]$XMin[interval]
          }
)

#' @rdname TextGrid
setMethod(f          = 'GetIntervalXMax',
          signature  = c(textGrid = 'TextGrid', tier = 'ANY', interval = 'numeric'),
          definition = function(textGrid, tier, interval) {
            textGrid[[tier]]$XMax[interval]
          }
)


