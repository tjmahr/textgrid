
## Slot Getters

# TextGrid / IntervalTier / TextTier

#' Get the size of Praat Textgrid or Tier
#' @export
setGeneric(name = 'size',
           def = function(.Object) {standardGeneric('size')}
)

#' Get starting time of a Praat Textrid or Tier
#' @export
setGeneric(name = 'startTime',
           def = function(.Object) {standardGeneric('startTime')}
)

#' Get ending time of a Praat Textrid or Tier
#' @export
setGeneric(name = 'endTime',
           def = function(.Object) {standardGeneric('endTime')}
)

#' Get unit of measurement for time
#' @export
setGeneric(name = 'timeUnit',
           def = function(.Object) {standardGeneric('timeUnit')}
)

#' Get tier name
#' @export
setGeneric(name = 'tierName',
           def = function(object) {standardGeneric('tierName')}
)


# IntervalTier / TextTier

#' Get tier number
#' @export
setGeneric(name = 'tierNumber',
           def = function(.Object) {standardGeneric('tierNumber')}
)


# TextGrid

#' @rdname TextGrid
#' @export
setGeneric(name = 'textGridName',
           def = function(.Object) {standardGeneric('textGridName')}
)








## Slot Setters

# Shared

# TextGrid

setGeneric(name = 'textGridName<-',
           def = function(.Object, value) {standardGeneric('textGridName<-')}
)
setGeneric(name = 'size<-',
           def = function(.Object, value) {standardGeneric('size<-')}
)
setGeneric(name = 'startTime<-',
           def = function(.Object, value) {standardGeneric('startTime<-')}
)
setGeneric(name = 'endTime<-',
           def = function(.Object, value) {standardGeneric('endTime<-')}
)
setGeneric(name = 'timeUnit<-',
           def = function(.Object, value) {standardGeneric('timeUnit<-')}
)

# IntervalTier

setGeneric(name = 'tierNumber<-',
           def = function(.Object, value) {standardGeneric('tierNumber<-')}
)
setGeneric(name = 'tierName<-',
           def = function(.Object, value) {standardGeneric('tierName<-')}
)
setGeneric(name = 'size<-',
           def = function(.Object, value) {standardGeneric('size<-')}
)
setGeneric(name = 'startTime<-',
           def = function(.Object, value) {standardGeneric('startTime<-')}
)
setGeneric(name = 'endTime<-',
           def = function(.Object, value) {standardGeneric('endTime<-')}
)
setGeneric(name = 'timeUnit<-',
           def = function(.Object, value) {standardGeneric('timeUnit<-')}
)

# TextTier

setGeneric(name = 'tierNumber<-',
           def = function(.Object, value) {standardGeneric('tierNumber<-')}
)
setGeneric(name = 'tierName<-',
           def = function(.Object, value) {standardGeneric('tierName<-')}
)
setGeneric(name = 'size<-',
           def = function(.Object, value) {standardGeneric('size<-')}
)
setGeneric(name = 'startTime<-',
           def = function(.Object, value) {standardGeneric('startTime<-')}
)
setGeneric(name = 'endTime<-',
           def = function(.Object, value) {standardGeneric('endTime<-')}
)
setGeneric(name = 'timeUnit<-',
           def = function(.Object, value) {standardGeneric('timeUnit<-')}
)


## Methods

# Shared

# TextGrid

setGeneric(name = 'FormatAsPraatText',
           def = function(x, ...) {standardGeneric('FormatAsPraatText')}
)

setGeneric(name = 'TimeSlice',
           def = function(x, sliceFrom, sliceTo, sliceUnit = timeUnit(x), ...) {
             standardGeneric('TimeSlice')
           }
)
setGeneric(name = 'GetIntervalText',
           def = function(textGrid, tier, interval, ...) {
             standardGeneric('GetIntervalText')
           }
)
setGeneric(name = 'GetIntervalXMin',
           def = function(textGrid, tier, interval, ...) {
             standardGeneric('GetIntervalXMin')
           }
)
setGeneric(name = 'GetIntervalXMax',
           def = function(textGrid, tier, interval, ...) {
             standardGeneric('GetIntervalXMax')
           }
)

# IntervalTier
setGeneric(name = 'FormatAsPraatText',
           def = function(x, ...) {standardGeneric('FormatAsPraatText')}
)
setGeneric(name = 'TimeSlice',
           def = function(x, sliceFrom, sliceTo, sliceUnit = timeUnit(x), ...) {
             standardGeneric('TimeSlice')
           }
)


# TextTier

setGeneric(name = 'FormatAsPraatText',
           def = function(x, ...) {standardGeneric('FormatAsPraatText')}
)
setGeneric(name = 'TimeSlice',
           def = function(x, sliceFrom, sliceTo, sliceUnit = timeUnit(x), ...) {
             standardGeneric('TimeSlice')
           }
)

