# example3: a time series with 2 clusters of CPs
# Note: a cluster is composed of 2 or more close CPs
# it can be due to noise spikes or unmodelled transients signals
# the Cluster_screening() function is used to detect clusters and provide
# information for the subsequent screening of the segmentation results with
# the UpdatedParametersForFixedCP() function
# for further details cf. (Quarello et al., 2022).

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
n = 1000                                       # length of time series
cp_ind <- c(10, 200, 210, 580, 590, 600, 990)  # 2 clusters of CPs + one short segment at the begining and one at the end => 7 CPs
segmt_mean <- c(0, -1, 5, 1, -5, 5, 2, 0)      # mean of segments
noise_stdev = 1                                # noise std dev
true_cp_ind <- c(200, 600)                     # only 2 true CPs 

# create a time series df
set.seed(1)                 # initialise random generator
mydate <- seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-01-01")+(n-1), by = "day")
mysignal <- simulate_time_series(cp_ind, segmt_mean, noise_stdev, n)
df = data.frame(date = mydate, signal = mysignal)

# Create metadata df for the true CP positions
true_cp_df = data.frame(date = df$date[true_cp_ind], type = rep("True", (length(true_cp_ind))))

# plot signal
plot(df$date, df$signal, type = "l",xlab ="Date",ylab="signal")

# run segmentation
seg = Segmentation(OneSeries = df, FunctPart = FALSE)
seg
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = FALSE)

# Note: the segmentation detects all CPs

# cluster screening 
cluster_max_dist = 80             # max distance between CPs in a cluster
screening = Cluster_screening(Tmu = seg$Tmu, MaxDist = cluster_max_dist)
screening

# update the segmentation parameters
seg_updated = UpdatedParametersForFixedCP(OneSeries = df, ResScreening = screening, FunctPart=FALSE)
seg_updated

# validate updated CP position wrt metadata
valid_max_dist = 62               # max distance between CP and metadata for the validation
valid = Validation(OneSeries = df, Tmu = seg_updated$Tmu, MaxDist = valid_max_dist, Metadata = true_cp_df)
valid

# Plot the time series with RemoveData option
PlotSeg(OneSeries = df, SegRes = seg_updated, FunctPart = FALSE, RemoveData = screening$RemoveData, Metadata = true_cp_df, Validated_CP_Meta = valid)

# Note: the data in the clusters are not shown by the Plot function with the RemoveData option, but they are still in the time series

# Screen also the time series before further use
df_screened <- df
for (i in 1:(length(screening$RemoveData))) {
    df_screened$signal[screening$RemoveData$begin[i]:screening$RemoveData$end[i]] = NA
}
PlotSeg(OneSeries = df_screened, SegRes = seg_updated, FunctPart = FALSE, Metadata = true_cp_df, Validated_CP_Meta = valid)
