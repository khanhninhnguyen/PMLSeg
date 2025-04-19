## Example 1: time series with 2 change-points and constant noise

### 1. Simulate a time series

rm(list=ls(all=TRUE))
library(PMLseg)

# Note 1: by convention the date/time of a change-point is the date/time of the last point in a the segment

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
noise_stdev <- 1             # noise std dev

# create a time series data frame
set.seed(1)                 # initialise random generator
mydate <- seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-01-01")+(n-1), by = "day")
mysignal <- simulate_time_series(cp_ind, segmt_mean, noise_stdev, n)
df <- data.frame(date = mydate, signal = mysignal)

# plot signal and position of change-points (red dashed line)
plot(df$date, df$signal, type = "l", xlab = "Date", ylab = "signal")
abline(v = mydate[cp_ind], col = "red", lty = 2)

### 2. Segmentation

# Run the segmentation with default parameters and no functional:
seg = Segmentation(OneSeries = df, FunctPart = FALSE)
str(seg)

### 3. Visualization of the time series with segmentation results superposed

PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE)

### 4. Validation of detected change-points with metadata

# For the example, we create a fake metadata data frame with the true position of change-points:
meta_ind = cp_ind               # index in time series of metadata information
meta_date <- df$date[meta_ind]  # corresponding date 
meta_type <- c("R", "RAD")      # type of information, e.g. R = receiver change, A = antenna change, D = radome change
metadata = data.frame(date = meta_date, type = meta_type)
metadata

# Plot with metadata:

PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE, Metadata = metadata)

# Validate estimated CP position wrt metadata
valid_max_dist = 10             # maximum distance wrt metadata for a CP to be validated
valid = Validation(OneSeries = df, Tmu = seg$Tmu, MaxDist =  valid_max_dist, Metadata = metadata)
valid

# Note: valid$Distance gives the distance between estimated CP and metadata

# plot with metadata and validation results
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE, Metadata = metadata, Validated_CP_Meta = valid)

# Validated change-points are inducated by a filled triangle at the bottom line.

### 5. Further explore the sensitivity of segmentation results to signal and noise parameters

### 5. Further explore the sensitivity of segmentation results to signal and noise parameters

#### Impact of sample size on estimated parameters

#1. run again with `n = 4000` and observe that:
#- the position of change-points is the same as with `n = 1000`
#- the `seg$Tmu$mean` value the last segment (longer than with `n = 1000`) is more accurate and `seg$Tmu$se` is smaller
#- the `seg$MonthVar` values are closer to the true value (1)

#2. run again with `n = 500`

#### Impact of signal to noise ratio on estimated parameters

#3. run again with `noise_std = 0.1` and observe that the the `seg$Tmu$mean` and `seg$MonthVar estimates are more precise.
  
#4. run again with `noise_std = 10` and observe the opposite.
