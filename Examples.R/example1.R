# example1: simple time series with 2 change-points (CPs)
#           IID noise, and no periodic bias
#
# Note: by convention the position of a change-point is the last point in the segment

rm(list=ls(all=TRUE))
library(PMLseg)

# time series simulation function
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

# specify time series simulation parameters
n = 1000                    # length of time series
cp_ind <- c(200, 600)       # position of change points (index in time series)
segmt_mean <- c(-1, 1, 2)   # mean value of segments
noise_stdev = 1             # noise std dev

# create a time series data frame
set.seed(1)                 # initialise random generator
mydate <- seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-01-01")+(n-1), by = "day")
mysignal <- simulate_time_series(cp_ind, segmt_mean, noise_stdev, n)
df = data.frame(date = mydate, signal = mysignal)
plot(df$date, df$signal, type = "l",xlab ="Date",ylab="signal")

# run segmentation
seg = Segmentation(OneSeries = df, FunctPart = FALSE)
seg

MonthSig=sqrt(seg$MonthVar)
mean(MonthSig)
sd(MonthSig)

# plot time series with segmentation results superposed
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE)

# Compare estimated CP position to truth
est_cp_ind <- seg$Tmu$end[1:(length(seg$Tmu$end)-1)]
est_cp_ind - cp_ind

# Create fake metadata df
meta_ind <- c(100, 600)         # position of metadata (index in time series)
meta_date <- df$date[meta_ind]  # date of metadata
meta_type <- c("R", "RAD")      # type of event, e.g. R = receiver change, A = antenna change, D = radome change
metadata = data.frame(date = meta_date, type = meta_type)
metadata

# Validate estimated CP position wrt metadata
valid_max_dist = 10             # validation parameter
valid = Validation(OneSeries = df, Tmu = seg$Tmu, MaxDist = valid_max_dist, Metadata = metadata)
valid

# Note: valid$Distance gives the distance between estimated CP and metadata

# plot with metadata
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE, Metadata = metadata)

# plot with metadata and validation results
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE, Metadata = metadata, Validated_CP_Meta = valid)

# Tip: for the validation of simulated data, define a metadata df with the true positions of CPs to evaluate the accuracy of the estimated CPs

# Create a metadata df with the true CP positions
truth = data.frame(date = df$date[cp_ind], type = rep("True", (length(cp_ind))))

# Evaluate the estimated CP position wrt truth
valid = Validation(OneSeries = df, Tmu = seg$Tmu, MaxDist = valid_max_dist, Metadata = truth)
valid

# plot with metadata=truth
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE, Metadata = truth)

# Further explore the sensitivity of segmentation results to signal and noise parameters
# * Impact of sample size on estimated parameters
#    a) run again with n = 4000 and observe that:
#    - the position of change-points is the same as with n=1000
#    - the seg$Tmu$mean value the last segment (longer than with n=1000) is more accurate and seg$Tmu$se is smaller
#    - the seg$MonthVar values are closer to the true value (1)
#    b) run again with n = 500
# * Impact of noise_std
#    c) run again with noise_std = 0.1
#    d) run again with noise_std = 10

