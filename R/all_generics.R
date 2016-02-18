
## Slot Getters

# Shared


# TextGrid
setGeneric(name = 'textGridName',
           def = function(.Object) {standardGeneric('textGridName')}
)
setGeneric(name = 'size',
           def = function(.Object) {standardGeneric('size')}
)
setGeneric(name = 'startTime',
           def = function(.Object) {standardGeneric('startTime')}
)
setGeneric(name = 'endTime',
           def = function(.Object) {standardGeneric('endTime')}
)
setGeneric(name = 'timeUnit',
           def = function(.Object) {standardGeneric('timeUnit')}
)

# IntervalTier

setGeneric(name = 'startTime',
           def = function(.Object) {standardGeneric('startTime')}
)
setGeneric(name = 'endTime',
           def = function(.Object) {standardGeneric('endTime')}
)
setGeneric(name = 'size',
           def = function(.Object) {standardGeneric('size')}
)
setGeneric(name = 'tierName',
           def = function(.Object) {standardGeneric('tierName')}
)
setGeneric(name = 'tierNumber',
           def = function(.Object) {standardGeneric('tierNumber')}
)
setGeneric(name = 'timeUnit',
           def = function(.Object) {standardGeneric('timeUnit')}
)

# TextTier

setGeneric(name = 'tierNumber',
           def = function(.Object) {standardGeneric('tierNumber')}
)
setGeneric(name = 'tierName',
           def = function(.Object) {standardGeneric('tierName')}
)
setGeneric(name = 'size',
           def = function(.Object) {standardGeneric('size')}
)
setGeneric(name = 'startTime',
           def = function(.Object) {standardGeneric('startTime')}
)
setGeneric(name = 'endTime',
           def = function(.Object) {standardGeneric('endTime')}
)
setGeneric(name = 'timeUnit',
           def = function(.Object) {standardGeneric('timeUnit')}
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

