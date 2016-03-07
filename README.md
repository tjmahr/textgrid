
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
#> [1] "Tier not found in 'alignment.TextGrid': 'null'"
validate_that(has_tiers(textgrid, c("null1", NA)))
#> [1] "Tiers not found in 'alignment.TextGrid': 'null1', 'NA'"

# assert_that returns TRUE or an error. This form is used inside of functions or
# scripts to make sure assumptions hold before executing additional code. I have
# to wrap this example with `try` so that the failure here does not crash the
# generation of this report.
try(assert_that(has_tiers(textgrid, "null")))
#> Error: Tier not found in 'alignment.TextGrid': 'null'
```

#### is\_interval\_tier, is\_text\_tier

Test that a given tier is an interval tier or a text tier

``` r
mixed_tiers <- TextGrid("tests/testthat/assets/tones.TextGrid")
mixed_tiers
#> Textgrid: tones.TextGrid
#> Start time: 0 (seconds)
#> End time: 10 (seconds)
#> Tiers: 4
#>   1: IntervalTier 'm' with 2 intervals
#>   2: IntervalTier 'j' with 3 intervals
#>   3: TextTier 'b' with 2 texts
#>   4: TextTier 'd' with 2 texts

is_text_tier(mixed_tiers, "b")
#> [1] TRUE
is_text_tier(mixed_tiers, "m")
#> [1] FALSE

is_interval_tier(mixed_tiers, "m")
#> [1] TRUE
is_interval_tier(mixed_tiers, "b")
#> [1] FALSE

# Custom failure messages
validate_that(is_interval_tier(mixed_tiers, c("b")))
#> [1] "The following tier is not an interval tier in 'tones.TextGrid': 'b'"
validate_that(is_text_tier(mixed_tiers, c("m", "j")))
#> [1] "The following tiers are not text tiers in 'tones.TextGrid': 'm', 'j'"

# has_tiers() is a prerequisite for these functions
validate_that(is_interval_tier(mixed_tiers, NA))
#> [1] "Tier not found in 'tones.TextGrid': 'NA'"
```
