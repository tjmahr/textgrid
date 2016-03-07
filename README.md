
<!-- README.md is generated from README.Rmd. Please edit that file -->
textgrid
========

installation
------------

First install the devtools package, then install from this repository.

``` r
devtools::install_github("tjmahr/textgrid")
```

usage
-----

Load a textgrid.

``` r
library("textgrid")
textgrid <- TextGrid("tests/testthat/assets/alignment.TextGrid")
textgrid
#> Textgrid: alignment.TextGrid
#> Start time: 0 (seconds)
#> End time: 0.4 (seconds)
#> Tiers: 3
#>   1: IntervalTier 'word' with 3 intervals
#>   2: IntervalTier 'label' with 3 intervals
#>   3: IntervalTier 'segment' with 6 intervals
```

Extract a tier as a dataframe

``` r
data.frame(textgrid[["word"]])
#>   TierNumber TierName TierType TimeUnit       XMin       XMax Text
#> 1          1     word Interval  seconds 0.00000000 0.02244698 <NA>
#> 2          1     word Interval  seconds 0.02244698 0.07152908 flwr
#> 3          1     word Interval  seconds 0.07152908 0.40000000 <NA>
```

Convert textgrid into a dataframe.

``` r
data.frame(textgrid)
#>              TextGrid TierNumber TierName TierType TimeUnit       XMin
#> 1  alignment.TextGrid          1     word Interval  seconds 0.00000000
#> 2  alignment.TextGrid          1     word Interval  seconds 0.02244698
#> 3  alignment.TextGrid          1     word Interval  seconds 0.07152908
#> 4  alignment.TextGrid          2    label Interval  seconds 0.00000000
#> 5  alignment.TextGrid          2    label Interval  seconds 0.02244698
#> 6  alignment.TextGrid          2    label Interval  seconds 0.07152908
#> 7  alignment.TextGrid          3  segment Interval  seconds 0.00000000
#> 8  alignment.TextGrid          3  segment Interval  seconds 0.02244698
#> 9  alignment.TextGrid          3  segment Interval  seconds 0.02954319
#> 10 alignment.TextGrid          3  segment Interval  seconds 0.04787506
#> 11 alignment.TextGrid          3  segment Interval  seconds 0.05704099
#> 12 alignment.TextGrid          3  segment Interval  seconds 0.07152908
#>          XMax   Text
#> 1  0.02244698   <NA>
#> 2  0.07152908   flwr
#> 3  0.40000000   <NA>
#> 4  0.02244698   <NA>
#> 5  0.07152908 flower
#> 6  0.40000000   <NA>
#> 7  0.02244698   <NA>
#> 8  0.02954319      f
#> 9  0.04787506   <NA>
#> 10 0.05704099      w
#> 11 0.07152908   <NA>
#> 12 0.40000000   <NA>
```

### textgrid validation

The package provides functions that can be used to validate assumptions about a textgrid.

These can be used on their own to get TRUE/FALSE values, or used with [assertthat](https://github.com/hadley/assertthat) functions so that failures return informative messages.

#### has\_tiers

Test that a textgrid contains certain tiers:

``` r
# Plain use
has_tiers(textgrid, "word")
#> [1] TRUE
has_tiers(textgrid, "null")
#> [1] FALSE

library("assertthat")
# validate_that returns TRUE or a string containing a failure message. It's used
# to demonstrate assertions.
validate_that(has_tiers(textgrid, "word"))
#> [1] TRUE
validate_that(has_tiers(textgrid, "null"))
#> [1] "The tier 'null' does not exist in the 'alignment.TextGrid'"

# assert_that returns TRUE or an error. This form is used inside of functions or
# scripts to make sure assumptions hold before executing additional code. I have
# to wrap this example with `try` so that the failure here does not crash the
# generation of this report.
try(assert_that(has_tiers(textgrid, "null")))
#> Error: The tier 'null' does not exist in the 'alignment.TextGrid'
```
