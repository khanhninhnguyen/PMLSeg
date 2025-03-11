#' update the segmentation parameters from the screening result
#'
#' @param OneSeries a data frame, with size n x 2, containing the signal with n points and the dates in Date format. The names of the 2 columns are thus signal and date
#' @param ResScreening the output of the Cluster_screening function
#' @param FunctPart a boolean indicating if the functional part is taking into account in the model. Default is TRUE and note that if \code{FunctPart=FALSE}, only a segmentation is performed
#' @param selectionF a boolean indicating if a selection on the functions of the Fourier decomposition of order 4 is performed. The level of the test is by default 0.001. Default is FALSE
#'
#' @return A file containing
#' \itemize{
#' \item \code{Tmu} that is a data frame containing the estimation of the segmentation parameters: for each segment, we get the beginning and the end positions (time index), the estimated mean (mean), the standard deviation of the mean (se) and the number of "valid" points (np) i.e. non-NA values in the segment
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

  # UpdatedData <- na.omit(UpdatedData)

  # Calculation of the monthly variances
  UpdatedData$year <- as.factor(format(UpdatedData$date,format='%Y'))
  UpdatedData$month <- as.factor(format(UpdatedData$date,format='%m'))
  UpdatedData$month <-  droplevels(UpdatedData$month)
  UpdatedData$year   <-  droplevels(UpdatedData$year)

  # New estimation of the monthly variances
  MonthVar <- RobEstiMonthlyVariance(UpdatedData)^2
  var.est.t = MonthVar[as.numeric(UpdatedData$month)]
  var.est.t[which(is.na(UpdatedData$signal))] <- NA


  # New estimation of the Tmu
  if(length(ResScreening$UpdatedCP)>0){
    begin = c(1, ResScreening$UpdatedCP + 1)
    end =  c(ResScreening$UpdatedCP, nrow(OneSeries))
  } else{
    begin = 1
    end = nrow(OneSeries)
  }

  # update according to the result of screening
  UpdatedCP = which(UpdatedData$date %in% OneSeries$date[ResScreening$UpdatedCP])
  UpdatedCP <- c(UpdatedCP, nrow(UpdatedData))

  # New estimation of f
  if (FunctPart==TRUE){
    lyear <- 365.25

    res.funct <- c()
    res.funct = periodic_estimation_all(Data = UpdatedData,
                                          CP = UpdatedCP,
                                          var.est.t = var.est.t,
                                          lyear = 365.25)

    funct <- res.funct$predict
    coeff <- res.funct$coeff

    auxilary_Data <- UpdatedData
    auxilary_Data$signal <- auxilary_Data$signal-funct
    Tmu <- FormatOptSegK(UpdatedCP,auxilary_Data,var.est.t)
    mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))+funct

  } else {
    Tmu <- FormatOptSegK(UpdatedCP,UpdatedData,var.est.t)
    mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))

    funct <- FALSE
    coeff <- FALSE
    mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))
  }


  UpdatePara <- c()
  UpdatePara$MonthVar <- MonthVar
  UpdatePara$Tmu   <-  Tmu
  UpdatePara$FitF  <-  funct
  UpdatePara$CoeffF <-  coeff
  UpdatePara$SSR <- sum(((UpdatedData$signal-mean.est.t)^2)/var.est.t,na.rm=TRUE)
<<<<<<< HEAD

=======
  
>>>>>>> 1ece07afaebf5d3c2a357a50e6a279af8ffae05e
  return(UpdatePara)

}


periodic_estimation_all = function(Data,CP,var.est.t,lyear){
  DataF=Data
  DataF$t=c(as.numeric(DataF$date-DataF$date[1]))+1
  for (i in 1:4){
    cosX=cos(i*DataF$t*(2*pi)/lyear)
    sinX=sin(i*DataF$t*(2*pi)/lyear)
    DataF=cbind(DataF,cosX,sinX)
    colnames(DataF)[(dim(DataF)[2]-1):(dim(DataF)[2])]=c(paste0('cos',i),paste0('sin',i))
  }

  # Add the new columns represent the mean to the dataframe
  new_columns <- lapply(1:length(CP), function(i) {
    new_col <- rep(0, nrow(DataF))
    if (i == 1) {
      start_index <- 1
      end_index <- CP[i]
    } else {
      start_index <- CP[i - 1] + 1
      end_index <- CP[i]
    }
    new_col[start_index:end_index] <- 1
    return(new_col)
  })

  names(new_columns) <- paste0("mean_", 1:length(CP))
  DataF <- DataF %>%
    select(signal,cos1, sin1, cos2, sin2, cos3, sin3, cos4, sin4)
  DataF <- cbind(DataF, new_columns)

  reg=stats::lm(signal~-1+.,weights=1/var.est.t,data=DataF)
  coeff=base::summary(reg)$coefficients[,1]
  result=list()
  result$predict=stats::predict(reg,DataF)
  result$coeff=coeff
  return(result)
}
