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

# run segmentation
seg = Segmentation(OneSeries = df, FunctPart = FALSE)
seg

seg$Tmu

PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE)

# Create metadata for the true CP positions
meta_ind <- cp_ind               # index in time series of metadata information
meta_date <- df$date[meta_ind]  # corresponding date 
meta_type <- c("true", "true")      # type of information, e.g. R = receiver change, A = antenna change, D = radome change
metadata <- data.frame(date = meta_date, type = meta_type)
metadata

# validation of estimated CP position wrt truth
valid_max_dist = 62         # validation parameter
valid_max_dist <- 10             # maximum distance wrt metadata for a CP to be validated
valid <- Validation(OneSeries = df, 
           Tmu = seg$Tmu,
           MaxDist =  valid_max_dist,
           Metadata = metadata)
valid

# plot 
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE, Metadata = truth, Validated_CP_Meta = valid)

# Note: the segmentation works well despite the steep variations in the noise variance

# Compare estimated noise variance vs truth
error <- seg$MonthVar - noise_stdev ** 2
error / noise_stdev

# Note: increasing the length of the time series (more years) would improve the accuracy of the noise variance estimation
