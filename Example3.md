<!-- Example3.md is generated from Example3.Rmd. Please edit that file -->

## Example 1: time series clusters of CPs

### 1. Simulate a time series with 2 clusters

    rm(list=ls(all=TRUE))
    library(PMLseg)

    # time series simulation function
    # Note: by convention the position of a change-point is the last point in the segment
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
    n = 1000                    # length of time series
    cp_ind <- c(10, 200, 210, 580, 590, 600, 990)  # 2 clusters of CPs + one short segment at the begining and one at the end => 7 CPs
    segmt_mean <- c(-1, 1, 2)   # mean value of segments
    noise_stdev = 1             # noise std dev (identical for all months)
    set.seed(1)                 # initialise random generator

    # create a data frame of time series with 2 columns: date, signal
    mydate <- seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-01-01")+(n-1), by = "day")
    mysignal <- simulate_time_series(cp_ind, segmt_mean, noise_stdev, n)
    #> Warning in changes[jump_indices[-length(jump_indices)]] <- offsets: le nombre
    #> d'objets à remplacer n'est pas multiple de la taille du remplacement
    df = data.frame(date = mydate, signal = mysignal)
    plot(df$date, df$signal, type = "l",xlab ="Date",ylab="signal")

<img src="Example3_files/figure-markdown_strict/unnamed-chunk-2-1.png" width="100%" />

    head(df, 3)
    #>         date     signal
    #> 1 2010-01-01 -1.6264538
    #> 2 2010-01-02 -0.8163567
    #> 3 2010-01-03 -1.8356286

### 2. Segmentation

Run the segmentation with default parameters and no functional:

    seg = Segmentation(OneSeries = df, 
                       FunctPart = FALSE)
    str(seg)
    #> List of 5
    #>  $ Tmu     :'data.frame':    5 obs. of  5 variables:
    #>   ..$ begin: int [1:5] 1 11 201 582 991
    #>   ..$ end  : int [1:5] 10 200 581 990 1000
    #>   ..$ mean : num [1:5] -0.868 1.035 1.999 4.967 6.674
    #>   ..$ se   : num [1:5] 0.3352 0.0771 0.055 0.0528 0.2877
    #>   ..$ np   : int [1:5] 10 190 381 409 10
    #>  $ FitF    : logi FALSE
    #>  $ CoeffF  : logi FALSE
    #>  $ MonthVar: num [1:12] 1.124 0.887 1.334 1.092 1.21 ...
    #>  $ SSR     : num 922

    seg$Tmu
    #>   begin  end       mean         se  np
    #> 1     1   10 -0.8677972 0.33523832  10
    #> 2    11  200  1.0353245 0.07708981 190
    #> 3   201  581  1.9985557 0.05504727 381
    #> 4   582  990  4.9668490 0.05282113 409
    #> 5   991 1000  6.6737683 0.28772551  10

### 3. Visualization of the time series with segmentation results superposed

    PlotSeg(OneSeries = df, 
            SegRes = seg, 
            FunctPart = FALSE)

<img src="Example3_files/figure-markdown_strict/unnamed-chunk-5-1.png" width="100%" />

### 4. Cluster screening

cluster\_max\_dist = 80 \# max distance between CPs in a cluster
screening = Cluster\_screening(Tmu = seg$Tmu, MaxDist =
cluster\_max\_dist) screening

# update the segmentation parameters

seg\_updated = UpdatedParametersForFixedCP(OneSeries = df, ResScreening
= screening, FunctPart=FALSE) seg\_updated

# Plot the time series with RemoveData option

PlotSeg(OneSeries = df, SegRes = seg\_updated, FunctPart = FALSE,
RemoveData = screening$RemoveData)
