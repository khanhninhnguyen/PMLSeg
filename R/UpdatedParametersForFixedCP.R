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


UpdatedParametersForFixedCP <- function(OneSeries,ResScreening,ResSeg,FunctPart=TRUE,selectionF=FALSE){

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
  n.UpdatedData <- nrow(UpdatedData)

  # Calculation of the monthly variances
  UpdatedData$year <- as.factor(format(UpdatedData$date,format='%Y'))
  UpdatedData$month <- as.factor(format(UpdatedData$date,format='%m'))
  UpdatedData$month <-  droplevels(UpdatedData$month)
  UpdatedData$year   <-  droplevels(UpdatedData$year)

  # New estimation of the monthly variances
  MonthVar <- RobEstiMonthlyVariance(UpdatedData)^2

  # New estimation of the Tmu
  UpdatedCP = which(UpdatedData$date %in% OneSeries$date[SegRes$Tmu$end])

  var.est.t = MonthVar[as.numeric(UpdatedData$month)]
  Tmu <- FormatOptSegK(c(UpdatedCP,n.UpdatedData),UpdatedData,var.est.t)
  mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))

  # New estimation of f
  if (FunctPart==TRUE){
    lyear <- 365.25
    threshold<- 0.001

    res.funct <- c()
    auxiliar_data <- UpdatedData
    auxiliar_data$signal <- auxiliar_data$signal - mean.est.t

    #Option for estimating f
    if (selectionF==TRUE){
      res.funct <- periodic_estimation_selb(auxiliar_data,var.est.t,lyear,threshold)

    } else{
      res.funct <- periodic_estimation_tot(auxiliar_data,var.est.t,lyear)
    }

    funct<-res.funct$predict
    coeff<-res.funct$coeff

  } else {
    funct<-FALSE
    coeff<-FALSE
  }

  UpdatePara <- c()
  UpdatePara$MonthVar <- MonthVar
  UpdatePara$Tmu   <-  Tmu
  UpdatePara$FitF  <-  funct
  UpdatePara$CoeffF <-  coeff

  UpdatePara$Tmu <- UpdatePara$Tmu[UpdatePara$Tmu$np != 0, ]
  UpdatePara$Tmu$end[nrow(UpdatePara$Tmu)] <- nrow(OneSeries)
  # Update new changepoints
  update_ind = which(Screening$UpdatedCP != UpdatePara$Tmu$end)

  UpdatePara$Tmu$end <- Screening$UpdatedCP
  for(i in update_ind){
    UpdatePara$Tmu$begin[i+1] <- Screening$UpdatedCP[i] + 1
  }

  return(UpdatePara)

}

