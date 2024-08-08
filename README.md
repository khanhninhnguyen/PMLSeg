
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PMLSeg

This package used to detects change-points in daily climatic time series
modeled by piecewise mean, periodic bias, and monthly variance.
Additionally, it includes functions to validate and visualize the
segmentation results.

## Table of Contents

- [Installation](#installation)
- [Examples](#examples)

## Installation

You can install the development version of `PMLSeg` from GitHub with:

``` r

# Install devtools if you haven't already
install.packages("devtools")

# Install the package from GitHub
devtools::install_github("khanhninhnguyen/PMLSeg")
```

## Examples

1.  Creating a Time Series with Arbitrary Jumps

``` r
library(PMLseg)

set.seed(1)
length_series = 1000
df = data.frame(date = seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-01-01")+(length_series-1), by = "day"),  
                signal = rnorm(n = length_series, mean = 0, sd = 1))

plot(df$date, df$signal, type = "l")
```

<img src="README_files/figure-gfm/unnamed-chunk-3-1.png" width="100%" />

``` r
# Adding 2 arbitrary jumps 
jump_ind = sample(1:length_series, 2, replace = FALSE) 
for (i in jump_ind) {
  df$signal[1: i] <- df$signal[1: i] + 1
}
plot(df$date, df$signal, type = "l")
```

<img src="README_files/figure-gfm/unnamed-chunk-4-1.png" width="100%" />

2.  Segmenting the Time Series

Adjust the `FunctPart` parameter based on whether the series includes
seasonal mean components. This code run with default option for : `Kmax`
= 30 (maximum number of segments), `selectionK` = “BM_BJ” (recommended
penalized criterion), etc.

``` r
seg = Segmentation(OneSeries = df, 
                   FunctPart = FALSE)
str(seg)
#> List of 5
#>  $ Tmu     :'data.frame':    3 obs. of  5 variables:
#>   ..$ begin: int [1:3] 1 814 924
#>   ..$ end  : int [1:3] 813 923 1000
#>   ..$ mean : num [1:3] 1.9911 1.0773 -0.0453
#>   ..$ se   : num [1:3] 26.75 9.7 8.57
#>   ..$ np   : num [1:3] 813 110 77
#>  $ FitF    : logi FALSE
#>  $ CoeffF  : logi FALSE
#>  $ MonthVar: num [1:12] 1.089 0.887 1.322 1.092 1.21 ...
#>  $ SSR     : num 929
```

Main results: The beginning, end, mean value, standard deviation, and
number of points of segments are provided in the `Tmu` dataframe:

``` r
seg$Tmu
#>   begin  end        mean        se  np
#> 1     1  813  1.99109942 26.751349 813
#> 2   814  923  1.07725214  9.697921 110
#> 3   924 1000 -0.04528994  8.569465  77
```

3.  Visualizing Segmentation Results

``` r
PlotSeg(OneSeries = df, 
        SegRes = seg, 
        FunctPart = FALSE)
```

<img src="README_files/figure-gfm/unnamed-chunk-7-1.png" width="100%" />

4.  Validating Detected Change-Points with Metadata

Change-points are validated if the segmentation finds them within
`MinDist` points from the metadata. Default `MinDist` is 62.

``` r
# Create a metadata for example 
meta = data.frame(date = df$date[jump_ind],  
                type = c("example1", "example2"))
meta
#>         date     type
#> 1 2012-03-25 example1
#> 2 2012-07-11 example2
```

``` r
# Validate the segmentation result wrt metadata 
Validation(OneSeries = df, 
           Tmu = seg$Tmu,
           MinDist = 30 ,
           Metadata = meta)
#> # A tibble: 2 × 5
#>   CP         closestMetadata Distance type     valid
#>   <date>     <date>             <dbl> <chr>    <dbl>
#> 1 2012-03-23 2012-03-25             2 example1     1
#> 2 2012-07-11 2012-07-11             0 example2     1
```

5.  Detecting Clusters of Change-Points

Check for clusters of close change-points, which may due to outliers. In
this example, the last segment is shorter than the given threshold.

``` r
# Validate the segmentation result wrt metadata 
screening = Cluster_screening(Tmu = seg$Tmu,
                              MaxDist = 80)
```

Update variables after removal of short segments:

``` r
seg_upd = UpdatedParametersForFixedCP(OneSeries = df, 
                                      ResScreening = screening, 
                                      FunctPart = FALSE)
str(seg_upd)
#> List of 4
#>  $ MonthVar: num [1:12] 1.089 0.887 1.322 1.092 1.21 ...
#>  $ Tmu     :'data.frame':    2 obs. of  5 variables:
#>   ..$ begin: num [1:2] 1 814
#>   ..$ end  : num [1:2] 813 1000
#>   ..$ mean : num [1:2] 1.991 0.621
#>   ..$ se   : num [1:2] 26.5 12.7
#>   ..$ np   : num [1:2] 813 187
#>  $ FitF    : logi FALSE
#>  $ CoeffF  : logi FALSE
```

Visualize the final results:

``` r
PlotSeg(OneSeries = df, 
        SegRes = seg_upd, 
        FunctPart = FALSE)
```

<img src="README_files/figure-gfm/unnamed-chunk-12-1.png" width="100%" />
