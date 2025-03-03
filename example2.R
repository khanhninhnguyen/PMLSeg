# example2: time series with gaps (NA values)
#
# Note: the df$date must be continuous, gaps in the data must be introduced as NA's in df$signal

rm(list=ls(all=TRUE))

library(PMLseg)

simulate_time_series <- function(cp_ind, segmt_mean, noise_stdev, length_series) {
  time_series <- rep(0, length_series)
  cp_indices <- c(1, cp_ind+1, length_series + 1)
  offsets <- c(0, diff(segmt_mean))
  
  changes <- rep(0, length_series)
  changes[cp_indices[-length(cp_indices)]] <- offsets
  changes[1] <- segmt_mean[1]
  
  time_series <- cumsum(changes)
  noise <- rnorm(n = length_series, mean = 0, sd = noise_stdev)
  time_series <- time_series + noise
  
  return(time_series)
}

# specify time series simulation parameters and analysis parameters
n = 1000                     # length of time series
cp_ind <- c(200, 600)        # position of change points (index in time series)
segmt_mean <- c(-1, 1, 2)    # mean value of segments
noise_stdev = 1              # noise std dev

# create a time series df
set.seed(1)                 # initialise random generator
mydate <- seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-01-01")+(n-1), by = "day")
mysignal <- simulate_time_series(cp_ind, segmt_mean, noise_stdev, n)

# add NA's in the signal
NA_ind = seq(from = 100, to = 150, by = 1)  # gap in the 1st segment
mysignal[NA_ind] <- NA
NA_ind = seq(from = 580, to = 630, by = 1) # gap in the 2nd segment, superposed with 2nd change-point
mysignal[NA_ind] <- NA

# plot signal
df = data.frame(date = mydate, signal = mysignal)
plot(df$date, df$signal, type = "l")

# run segmentation
seg = Segmentation(OneSeries = df, FunctPart = FALSE)
seg

# plot segmentation results
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE)

# Create metadata for the true CP positions
truth = data.frame(date = df$date[cp_ind], type = rep("True", (length(cp_ind))))

# validate estimated CP position wrt metadata
valid_max_dist = 62               # max distance between CP and metadata for the validation
valid = Validation(OneSeries = df, Tmu = seg$Tmu, MinDist = valid_max_dist, Metadata = truth)
valid

# plot time series with segmentation results, validation
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE, Metadata = truth, Validated_CP_Meta = valid)
