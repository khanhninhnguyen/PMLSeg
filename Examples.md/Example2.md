<!-- Example2.md is generated from Example2.Rmd. Please edit that file -->

## Example 2: time series with missing data

### 1. Simulate time series

    rm(list=ls(all=TRUE))
    library(PMLseg)
    library(purrr)

    # Note 2: the time series must have continue time, missing data are included as NA values

    # define simulation function
    simulate_time_series <- function(cp_ind, segmt_mean, noise_stdev, length_series) {
      time_series <- rep(0, length_series)
      jump_indices <- c(1, cp_ind+1, length_series + 1)
      offsets <- c(0, diff(segmt_mean))

      changes <- rep(0, length_series)
      changes[jump_indices[-length(jump_indices)]] <- offsets
      changes[1] <- segmt_mean[1]

      time_series <- cumsum(changes)
      noise <- rnorm(n = length_series, mean = 0, sd = noise_stdev)
      time_series <- time_series + noise

      return(time_series)
    }

    # specify the simulation parameters
    n <- 1000                    # length of time series
    cp_ind <- c(200, 600)       # position of change points (index in time series)
    segmt_mean <- c(-1, 1, 2)   # mean value of segments
    noise_stdev <- 1             # noise std dev (identical for all months)
    set.seed(1)                 # initialise random generator

    # create a time series df
    mydate <- seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-01-01")+(n-1), by = "day")
    mysignal <- simulate_time_series(cp_ind, segmt_mean, noise_stdev, n)

    # add NA's in the signal
    NA_ind <- seq(from = 100, to = 150, by = 1)  # 1st gap
    mysignal[NA_ind] <- NA
    NA_ind <- seq(from = 580, to = 630, by = 1)  # 2nd gap 
    mysignal[NA_ind] <- NA
    df <- data.frame(date = mydate, signal = mysignal)

    # plot signal and position of change-points (red dashed line)
    plot(df$date, df$signal, type = "l",xlab ="Date",ylab="signal")
    abline(v = mydate[cp_ind], col = "red", lty = 2)

<img src="../Examples.md/Example2_files/figure-markdown_strict/unnamed-chunk-2-1.png" width="100%" />

Note that the 1st gap lies within the 1st segment, while the 2nd gap is
overlapping the 2nd and 3rd segments.

### 2. Segmentation

Run the segmentation with default parameters and no functional:

    seg = Segmentation(OneSeries = df, 
                       FunctPart = FALSE)
    seg$Tmu
    #>   begin  end       mean         se  np
    #> 1     1  200 -0.8974084 0.08695923 149
    #> 2   201  636  1.0046283 0.05436229 385
    #> 3   637 1000  1.9698991 0.05627932 364

The first CP is detected at the right position (index=200) but not the
2nd (index=636) because of the gap.

Note that `np` is the number of valid points, i.e. excluding the NA
values, in each segment.

### 3. Visualization of the time series with segmentation results superposed

    PlotSeg(OneSeries = df, 
            SegRes = seg, 
            FunctPart = FALSE)
    #> Warning: No shared levels found between `names(values)` of the manual scale and the
    #> data's shape values.
    #> No shared levels found between `names(values)` of the manual scale and the
    #> data's shape values.
    #> No shared levels found between `names(values)` of the manual scale and the
    #> data's shape values.

<img src="../Examples.md/Example2_files/figure-markdown_strict/unnamed-chunk-4-1.png" width="100%" />

### 4. Validation of detected change-points with metadata

For the example, we create a fake metadata data frame with the true
position of change-points:

    meta_ind = cp_ind               # index in time series of metadata information
    meta_date <- df$date[meta_ind]  # corresponding date 
    meta_type <- c("R", "RAD")      # type of information, e.g. R = receiver change, A = antenna change, D = radome change
    metadata = data.frame(date = meta_date, type = meta_type)
    metadata
    #>         date type
    #> 1 2010-07-19    R
    #> 2 2011-08-23  RAD

Plot with metadata:

    PlotSeg(OneSeries = df, 
            SegRes = seg, 
            FunctPart = FALSE, 
            Metadata = metadata) 

<img src="../Examples.md/Example2_files/figure-markdown_strict/unnamed-chunk-6-1.png" width="100%" />

Validate estimated change-point positions wrt metadata:

    valid_max_dist = 10             # maximum distance wrt metadata for a CP to be validated
    valid = Validation(OneSeries = df, 
               Tmu = seg$Tmu,
               MaxDist =  valid_max_dist,
               Metadata = metadata)
    valid
    #> # A tibble: 2 × 5
    #>   CP         closestMetadata Distance type  valid
    #>   <date>     <date>             <dbl> <chr> <dbl>
    #> 1 2010-07-19 2010-07-19             0 R         1
    #> 2 2011-09-28 2011-08-23             5 RAD       1

Note that the distance between the 2nd CP (index=636) and the nearest
metadata (index=600) excludes the NA values (up to 630): 636 - 631 = 5.

Hence both CPs are validated.

Plot with metadata and validation results:

    PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE, Metadata = metadata, Validated_CP_Meta = valid)

<img src="../Examples.md/Example2_files/figure-markdown_strict/unnamed-chunk-8-1.png" width="100%" />
