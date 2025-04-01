# example5: a time series with periodic bias

rm(list=ls(all=TRUE))

library(PMLseg)

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

# specify time series simulation parameters
date_begin <- as.Date("2010-03-01") # date of first data point
n = 1000                            # length of time series
cp_ind <- c(200, 600)               # position of change points (index in time series)
segmt_mean <- c(-1, 1, 2)           # mean value of segments
noise_stdev <- rep(0.01, 12)         # noise variance (Jan to Dec)
coeff <- c(1, 0, 0, 0)              # Fourier Series coefficients (cos1, sin1, cos2, sin2...) up to order 4

# create a time series df with jumps and noise
set.seed(1)
mydate <- seq.Date(from = date_begin, to = date_begin + n - 1, by = "day")
mysignal <- simulate_time_series(cp_ind, segmt_mean, noise_stdev, n)

# create a periodic function (Fourier series)
T = 365.25                             # reference period (unit days)
t = as.numeric(mydate)                 # time variable for periodic function
t0 = as.numeric(mydate[1])             # reference date of periodic function is first date of time series
p = length(coeff) / 2                  # order of Fourier Series
f = rowSums(sapply(1:p, function(i) coeff[2*i-1]*cos(i*(t-t0+1)*(2*pi)/T) + coeff[2*i]*sin(i*(t-t0+1)*(2*pi)/T)))
plot(mydate, f, type = "l")

# full signal
mysignal <- mysignal + f
df = data.frame(date = mydate, signal = mysignal)
plot(df$date, df$signal, type = "l")

# run segmentation
seg.SSR=c()
for (k in 2:30){
  seg = Segmentation(OneSeries = df, FunctPart = TRUE,selectionK="none",Kmax = k)
  seg.SSR[k]=seg$SSR
}

seg$Tmu
seg$CoeffF
seg$MonthVar
seg$SSR

sum(seg$CoeffF^2)

# run segmentation with selection of statistically significant Fourier coefficients
seg = Segmentation(OneSeries = df, FunctPart = TRUE, selectionF = TRUE)
seg$Tmu
seg$CoeffF
seg$MonthVar
seg$SSR

sum(seg$CoeffF^2)

PlotSeg(OneSeries = df, SegRes = seg, FunctPart = TRUE)
