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
grid <- TextGrid("tests/testthat/assets/tones.TextGrid")
grid
#> Textgrid: tones.TextGrid
#> Start time: 0 (seconds)
#> End time: 10 (seconds)
#> Tiers: 3
#>   1: IntervalTier 'm' with 2 intervals
#>   2: IntervalTier 'j' with 3 intervals
#>   3: TextTier 'b' with 2 texts
```

Extract a tier as a dataframe

``` r
data.frame(grid[["m"]])
#>   TierNumber TierName TierType TimeUnit     XMin      XMax   Text
#> 1          1        m Interval  seconds 0.000000  1.525628     b1
#> 2          1        m Interval  seconds 1.525628 10.000000 b2 end
```

Convert textgrid into a data-frame.

``` r
data.frame(grid)
#>         TextGrid TierNumber TierName TierType TimeUnit     XMin      XMax
#> 1 tones.TextGrid          1        m Interval  seconds 0.000000  1.525628
#> 2 tones.TextGrid          1        m Interval  seconds 1.525628 10.000000
#> 3 tones.TextGrid          2        j Interval  seconds 0.000000  1.999022
#> 4 tones.TextGrid          2        j Interval  seconds 1.999022  5.786172
#> 5 tones.TextGrid          2        j Interval  seconds 5.786172 10.000000
#> 6 tones.TextGrid          3        b     Text  seconds       NA        NA
#> 7 tones.TextGrid          3        b     Text  seconds       NA        NA
#>     Text     Time Mark
#> 1     b1       NA <NA>
#> 2 b2 end       NA <NA>
#> 3     NA       NA <NA>
#> 4   <NA>       NA <NA>
#> 5      1       NA <NA>
#> 6   <NA> 3.047251   b1
#> 7   <NA> 7.037284   b2
```
