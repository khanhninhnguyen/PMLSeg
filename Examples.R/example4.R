# example4: a time series with monthly varying variance

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
  
  noise <- rnorm(n = length_series, mean = 0, sd = 1)
  sd = noise_stdev[as.numeric(format(mydate, "%m"))]
  time_series <- time_series + noise * sd
  
  return(time_series)
}

# specify time series simulation parameters
date_begin <- as.Date("2010-01-01") # date of first data point
n = 1000                            # length of time series
cp_ind <- c(200, 600)               # position of change points (index in time series)
segmt_mean <- c(-1, 1, 2)           # mean value of segments
noise_stdev <- c(0.1, 0.3, 0.7, 1.2, 1.8, 2, 2, 1.8, 1.2, 0.7, 0.3, 0.1) # 12 values, one per month (Jan to Dec)

# create a time series df
set.seed(1)
mydate <- seq.Date(from = date_begin, to = date_begin + n - 1, by = "day")
mysignal <- simulate_time_series(cp_ind, segmt_mean, noise_stdev, n)
df = data.frame(date = mydate, signal = mysignal)
plot(df$date, df$signal, type = "l")

# run segmentation
seg = Segmentation(OneSeries = df, FunctPart = FALSE)
seg
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE)

# Create metadata for the true CP positions
truth = data.frame(date = df$date[cp_ind], type = rep("True", (length(cp_ind))))

# validation of estimated CP position wrt truth
valid_max_dist = 62         # validation parameter
valid = Validation(OneSeries = df, Tmu = seg$Tmu, MaxDist = valid_max_dist, Metadata = truth)
valid
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE, Metadata = truth, Validated_CP_Meta = valid)

# Note: the segmentation works well despite the steep variations in the noise variance

# Compare estimated noise variance vs truth
seg$MonthVar - noise_stdev ** 2
sd(seg$MonthVar - noise_stdev ^ 2)^2

# Note: increasing the length of the time series (more years) would improve the accuracy of the noise variance estimation
