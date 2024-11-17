#' update the segmentation parameters from the screening result
#'
#' @param OneSeries a data frame, with size n x 2, containing the signal with n points and the dates in Date format. The names of the 2 columns are thus signal and date
#' @param ResScreening the output of the Cluster_screening function
#' @param FunctPart a boolean indicating if the functional part is taking into account in the model. Default is TRUE and note that if \code{FunctPart=FALSE}, only a segmentation is performed
#' @param selectionF a boolean indicating if a selection on the functions of the Fourier decomposition of order 4 is performed. The level of the test is by default 0.001. Default is FALSE
#'
#' @return A file containing
#' \itemize{
#' \item \code{Tmu} that is a data frame containing the estimation of the segmentation parameters: for each segment, we get the beginning and the end positions (time index), the estimated mean (mean), the standard deviation of the mean (se) and the number of points (np)
#' \item \code{FitF} that corresponds to the estimation of the functional part. If \code{FunctPart=FALSE}, \code{FitF} is FALSE
#' \item \code{CoeffF} that corresponds to the estimation of the coefficients of the Fourier decomposition. The vector contains 8 coefficients if \code{selectionF=FALSE} or as many coefficients as the number of selected functions if \code{selectionF=TRUE}. If \code{FunctPart=FALSE}, \code{CoeffF} is FALSE
#' \item \code{MonthVar} that corresponds to the estimated variances of each fixed interval (each month)
#' }
#'
#' @export


UpdatedParametersForFixedCP <- function(OneSeries, ResScreening, FunctPart=TRUE, selectionF=FALSE){

  UpdatedData <- OneSeries

  if (ResScreening$ChangeCP=="No") {
    stop("Screening did not change segmentation results")
  }

  segments <- sapply(1:nrow(ResScreening$RemoveData), function(i) {
    ResScreening$RemoveData$begin[i]:ResScreening$RemoveData$end[i]
  })
  # Remove data in cluster
  for (seg in segments) {
    UpdatedData$signal[seg] <- NA
  }

  UpdatedData <- na.omit(UpdatedData)

  UpdatedData <- UpdatedData %>% tidyr::complete(date = seq(min(date), max(date), by = "day"))

  # Calculation of the monthly variances
  UpdatedData$year <- as.factor(format(UpdatedData$date,format='%Y'))
  UpdatedData$month <- as.factor(format(UpdatedData$date,format='%m'))
  UpdatedData$month <-  droplevels(UpdatedData$month)
  UpdatedData$year   <-  droplevels(UpdatedData$year)

  # New estimation of the monthly variances
  MonthVar <- RobEstiMonthlyVariance(UpdatedData)^2
  var.est.t = MonthVar[as.numeric(UpdatedData$month)]

  # New estimation of the Tmu
  UpdatedCP = which(UpdatedData$date %in% OneSeries$date[ResScreening$UpdatedCP])

  print(UpdatedCP)
  print(ResScreening$UpdatedCP)
  print(OneSeries$date[ResScreening$UpdatedCP])
  print(UpdatedData$date[UpdatedCP])

  # New estimation of f
  if (FunctPart==TRUE){
    lyear <- 365.25
    tol <- 0.001

    maxIter = 100
    Diff    = 2*tol
    Iter = 0

    res.funct <- c()
    auxiliar_data <- UpdatedData

    #Option for estimating f
    if (selectionF==TRUE){

      period <- periodic_estimation_tot_init(UpdatedData,var.est.t,lyear = 365.25)
      auxiliar_data$signal <- UpdatedData$signal - period$predict
      # res.funct <- periodic_estimation_tot(auxiliar_data,var.est.t,lyear)

      Tmu <- FormatOptSegK(UpdatedCP,auxiliar_data,var.est.t)
      mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))

      while ((Diff  > tol) & (Iter < maxIter)) {
        Iter = Iter +1

        auxiliar_data$signal = UpdatedData$signal - mean.est.t
        periodi = periodic_estimation_selb(auxiliar_data,var.est.t,lyear)

        auxiliar_data$signal = UpdatedData$signal - periodi$predict
        Tmu <- FormatOptSegK(UpdatedCP,auxiliar_data,var.est.t)
        mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))

        if (Iter == 2){
          t2 = c(period$predict, mean.est.t)
        }
        if (Iter == 3){
          t1 = c(period$predict, mean.est.t)
          t0 = c(periodi$predict,mean.est.t)
          tp0 = (t2-t1)/sum((t1==t2)+((t2-t1)^2)) + (t0-t1)/sum((t1==t0)+((t0-t1)^2))
          tp0 = t1 + tp0 / sum((tp0==0) + tp0^2)
        }
        if (Iter > 3){
          t2 = t1
          t1 = t0
          t0 = c(periodi$predict, mean.est.t)
          tp1 = tp0
          tp0 = (t2-t1)/sum((t1==t2)+((t2-t1)^2)) + (t0-t1)/sum((t1==t0)+((t0-t1)^2))
          tp0 = t1 + tp0 / sum((tp0==0) + tp0^2)
          Diff = sum((tp0-tp1)^2)
        }

        period = periodi
        Tmu = Tmu
      }
      res.funct <- period

    } else{

      period <- periodic_estimation_tot_init(UpdatedData,var.est.t,lyear = 365.25)
      auxiliar_data$signal <- UpdatedData$signal - period$predict
      # res.funct <- periodic_estimation_tot(auxiliar_data,var.est.t,lyear)

      Tmu <- FormatOptSegK(UpdatedCP,auxiliar_data,var.est.t)
      mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))

      maxIter = 100
      Diff    = 2*tol
      Iter = 0

      while ((Diff  > tol) & (Iter < maxIter)) {
        Iter = Iter +1

        auxiliar_data$signal = UpdatedData$signal - mean.est.t
        periodi = periodic_estimation_tot(auxiliar_data,var.est.t,lyear)

        auxiliar_data$signal = UpdatedData$signal - periodi$predict
        Tmu <- FormatOptSegK(UpdatedCP,auxiliar_data,var.est.t)
        mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))

        if (Iter == 2){
          t2 = c(period$predict, mean.est.t)
        }
        if (Iter == 3){
          t1 = c(period$predict, mean.est.t)
          t0 = c(periodi$predict,mean.est.t)
          tp0 = (t2-t1)/sum((t1==t2)+((t2-t1)^2)) + (t0-t1)/sum((t1==t0)+((t0-t1)^2))
          tp0 = t1 + tp0 / sum((tp0==0) + tp0^2)
        }
        if (Iter > 3){
          t2 = t1
          t1 = t0
          t0 = c(periodi$predict, mean.est.t)
          tp1 = tp0
          tp0 = (t2-t1)/sum((t1==t2)+((t2-t1)^2)) + (t0-t1)/sum((t1==t0)+((t0-t1)^2))
          tp0 = t1 + tp0 / sum((tp0==0) + tp0^2)
          Diff = sum((tp0-tp1)^2)
        }

        period = periodi
        Tmu = Tmu
      }
      res.funct <- period
    }

    funct <-res.funct$predict
    coeff <-res.funct$coeff

  } else {
    Tmu <- FormatOptSegK(UpdatedCP,UpdatedData,var.est.t)

    funct<-FALSE
    coeff<-FALSE
  }

  Tmu <- Tmu %>%
    mutate(begin = which(OneSeries$date %in% UpdatedData$date[Tmu$begin]),
           end = ResScreening$UpdatedCP,
           np = end - begin + 1)

  UpdatePara <- c()
  UpdatePara$MonthVar <- MonthVar
  UpdatePara$Tmu   <-  Tmu
  UpdatePara$FitF  <-  funct
  UpdatePara$CoeffF <-  coeff

  return(UpdatePara)

}

