<!-- Example4.md is generated from Example4.Rmd. Please edit that file -->

## Example 4: time series with monthly varying variance

### 1. Simulate a time series with one different variance every calendar month

    rm(list=ls(all=TRUE))
    library(PMLseg)
    library(purrr)

    # define simulation function
    simulate_time_series <- function(cp_ind, segmt_mean, noise_stdev, length_series) {
      time_series <- rep(0, length_series)
      jump_indices <- c(1, cp_ind+1, length_series + 1)
      offsets <- c(0, diff(segmt_mean))

      changes <- rep(0, length_series)
      changes[jump_indices[-length(jump_indices)]] <- offsets
      changes[1] <- segmt_mean[1]
      time_series <- cumsum(changes)

      noise <- rnorm(n = length_series, mean = 0, sd = 1)
      sd = noise_stdev[as.numeric(format(mydate, "%m"))]
      time_series <- time_series + noise * sd

      return(time_series)
    }

    # specify the simulation parameters
    n <- 1000                            # length of time series
    cp_ind <- c(200, 600)               # position of change points (index in time series)
    segmt_mean <- c(-1, 1, 2)           # mean value of segments
    noise_stdev <- c(0.1, 0.3, 0.7, 1.2, 1.8, 2, 2, 1.8, 1.2, 0.7, 0.3, 0.1) # 12 values, one per month (Jan to Dec)
    set.seed(1)                         # initialise random generator

    # create a data frame of time series with 2 columns: date, signal
    mydate <- seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-01-01")+(n-1), by = "day")
    mysignal <- simulate_time_series(cp_ind, segmt_mean, noise_stdev, n)
    df <- data.frame(date = mydate, signal = mysignal)

    # plot signal and position of change-points (red dashed line)
    plot(df$date, df$signal, type = "l",xlab ="Date",ylab="signal")
    abline(v = mydate[cp_ind], col = "red", lty = 2)

<img src="../Examples.md/Example4_files/figure-markdown_strict/unnamed-chunk-2-1.png" width="100%" />

### 2. Segmentation

Run the segmentation with default parameters:

    seg = Segmentation(OneSeries = df, 
                       FunctPart = FALSE)

    seg$Tmu
    #>   begin  end       mean         se  np
    #> 1     1  200 -0.9851706 0.01745298 200
    #> 2   201  601  0.9995062 0.01246875 401
    #> 3   602 1000  1.9948656 0.01244232 399

### 3. Validate estimated change-point positions wrt metadata:

    meta_ind <- cp_ind               # index in time series of metadata information
    meta_date <- df$date[meta_ind]  # corresponding date 
    meta_type <- c("true", "true")      # type of information, e.g. R = receiver change, A = antenna change, D = radome change
    metadata <- data.frame(date = meta_date, type = meta_type)
    metadata
    #>         date type
    #> 1 2010-07-19 true
    #> 2 2011-08-23 true

    valid_max_dist <- 10             # maximum distance wrt metadata for a CP to be validated
    valid <- Validation(OneSeries = df, 
               Tmu = seg$Tmu,
               MaxDist =  valid_max_dist,
               Metadata = metadata)
    valid
    #> # A tibble: 2 × 5
    #>   CP         closestMetadata Distance type  valid
    #>   <date>     <date>             <dbl> <chr> <dbl>
    #> 1 2010-07-19 2010-07-19             0 true      1
    #> 2 2011-08-24 2011-08-23             1 true      1

### 4. Visualization of the time series with segmentation and validation results superposed

    PlotSeg(OneSeries = df, 
            SegRes = seg, 
            FunctPart = FALSE,
            Metadata = metadata, 
            Validated_CP_Meta = valid)

<img src="../Examples.md/Example4_files/figure-markdown_strict/unnamed-chunk-5-1.png" width="100%" />

Note: the segmentation detects well the position of change points
despite the steep variations in the noise

### 5. Evaluate the error of the estimated noise variance

    error <- seg$MonthVar - noise_stdev ** 2
    error / (noise_stdev ** 2)
    #>  [1]  0.08875807 -0.11321115  0.33447764  0.09227653  0.20986374  0.17023340
    #>  [7]  0.18342544  0.22360029 -0.16599028  0.87456341  0.23758318  0.10628167

The relative error on some months is quite large.

Increasing the length of the time series (i.e. including more years)
would improve the accuracy of the noise variance estimation.
