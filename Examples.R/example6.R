# example6: a time series with periodic bias, monthly variance, and gaps

rm(list=ls(all=TRUE))

library(PMLseg)
#library(tidyr)
#source("E:\\0github\\PMLSeg\\R\\Segmentation.R")

simulate_time_series <- function(cp_ind, segmt_mean, noise_stdev, length_series) {
    time_series <- rep(0, length_series)
    cp_indices <- c(1, cp_ind, length_series + 1)
    offsets <- c(0, diff(segmt_mean))

    changes <- rep(0, length_series)
    changes[cp_indices[-length(cp_indices)]] <- offsets
    changes[1] <- segmt_mean[1]
    time_series <- cumsum(changes)

    sd = noise_stdev[as.numeric(format(mydate, "%m"))]
    noise <- rnorm(n = length_series, mean = 0, sd = 1)
    time_series <- time_series + noise * sd

    return(time_series)
}

fourier_series <- function(mydate, coeff) {
    T = 365.25                             # base period (unit days)
    t = as.numeric(mydate)                 # time variable for periodic function
    t0 = as.numeric(mydate[1])             # reference date of periodic function is first date of time series
    p = length(coeff) / 2                  # order of Fourier Series
    f = rowSums(sapply(1:p, function(i) coeff[2*i-1]*cos(i*(t-t0+1)*(2*pi)/T) + coeff[2*i]*sin(i*(t-t0+1)*(2*pi)/T)))

    return(f)
}


# specify time series simulation parameters
date_begin <- as.Date("2010-03-01")             # date of first data point
n = 1000                                        # length of time series
cp_ind <- c(200, 600)                           # position of CPs (index in time series)
segmt_mean <- c(-1, 1, 2)                       # mean value of segments
true_cp_ind <- c(200, 600)                      # only 2 true CPs 
noise_stdev <- c(0.1, 0.3, 0.7, 1.2, 1.8, 2, 2, 1.8, 1.2, 0.7, 0.3, 0.1) # 12 values, one per month (Jan to Dec)
coeff <- c(1, 0, 0, 0)                          # Fourier Series coefficients (cos1, sin1, cos2, sin2...) up to order 4
set.seed(1)                 # initialise random generator

# create a time series with jumps and noise
mydate <- seq.Date(from = date_begin, to = date_begin + n - 1, by = "day")
mysignal <- simulate_time_series(cp_ind, segmt_mean, noise_stdev, n)

# add NA's in the signal
NA_ind = seq(from = 100, to = 150, by = 1)  # 1st gap
mysignal[NA_ind] <- NA
NA_ind = seq(from = 580, to = 630, by = 1)  # 2nd gap 
mysignal[NA_ind] <- NA

# add a periodic function (Fourier series)
f <- fourier_series(mydate, coeff)
mysignal <- mysignal + f

# create df with full signal
df = data.frame(date = mydate, signal = mysignal)
plot(df$date, df$signal, type = "l")

# run segmentation with functional part
seg = Segmentation(OneSeries = df, FunctPart = TRUE)
seg$Tmu
seg$CoeffF
seg$MonthVar
seg$SSR
sum(seg$CoeffF^2)
PlotSeg(OneSeries = df, SegRes = seg, FunctPart = TRUE)

# run segmentation with selection of statistically significant Fourier coefficients
seg_selectF = Segmentation(OneSeries = df, FunctPart = TRUE, selectionF = TRUE)
seg_selectF$Tmu
seg_selectF$CoeffF
seg_selectF$MonthVar
seg_selectF$SSR
sum(seg$CoeffF^2)
PlotSeg(OneSeries = df, SegRes = seg_selectF, FunctPart = TRUE)

# validate updated CP position wrt metadata
true_cp_df = data.frame(date = df$date[true_cp_ind], type = rep("True", (length(true_cp_ind))))
valid_max_dist = 10               # max distance between CP and metadata for the validation
valid = Validation(OneSeries = df, Tmu = seg_selectF$Tmu, MaxDist = valid_max_dist, Metadata = true_cp_df)
valid

# Plot the time series with RemoveData option
PlotSeg(OneSeries = df, SegRes = seg_selectF, FunctPart = TRUE, Metadata = true_cp_df, Validated_CP_Meta = valid)


