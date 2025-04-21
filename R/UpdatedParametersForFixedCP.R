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
#' \item \code{SSR} is the Sum of Squared Residuals of the fit.
#' }
#'
#' @export


UpdatedParametersForFixedCP <- function(OneSeries, ResScreening, FunctPart=TRUE, selectionF=FALSE){

  UpdatedSeries <- OneSeries

  if (ResScreening$ChangeCP=="No") {
    warning("Screening did not change segmentation results")
    return(NULL)
  }

  # Remove data in cluster
  segments <- sapply(1:nrow(ResScreening$RemoveData), function(i) {
    ResScreening$RemoveData$begin[i]:ResScreening$RemoveData$end[i]
  })
  for (seg in segments) {
    UpdatedSeries$signal[seg] <- NA
  }

  # Estimate monthly variance parameters
  UpdatedSeries$year <- as.factor(format(UpdatedSeries$date,format='%Y'))
  UpdatedSeries$month <- as.factor(format(UpdatedSeries$date,format='%m'))
  UpdatedSeries$month <-  droplevels(UpdatedSeries$month)
  UpdatedSeries$year   <-  droplevels(UpdatedSeries$year)
  MonthVar <- RobEstiMonthlyVariance(UpdatedSeries)^2
  
  # Compute time series of monthly variance
  var.est.t = MonthVar[as.numeric(UpdatedSeries$month)]
  var.est.t[which(is.na(UpdatedSeries$signal))] <- NA

  # Update begin and end of segments
  if(length(ResScreening$UpdatedCP)>0){
    begin = c(1, ResScreening$UpdatedCP + 1)
    end =  c(ResScreening$UpdatedCP, nrow(OneSeries))
  } else{
    begin = 1
    end = nrow(OneSeries)
  }

  # Update CPs
  UpdatedCP = which(UpdatedSeries$date %in% OneSeries$date[ResScreening$UpdatedCP])
  UpdatedCP <- c(UpdatedCP, nrow(UpdatedSeries))
  # print(UpdatedCP)
  
  # Re-estimate means of segments and functional part
  if (FunctPart==TRUE){
    lyear <- 365.25
    coeff <- c()
    funct <- c()
    predicted_signal <- c()
    
    UpdatedSignalModel <- periodic_estimation_all(Data = UpdatedSeries, CP = UpdatedCP, var.est.t = var.est.t, lyear = lyear)

    predicted_signal <- UpdatedSignalModel$predict
    coeff <- UpdatedSignalModel$coeff
    funct <- UpdatedSignalModel$FitF
        
    Tmu <- FormatOptSegK(UpdatedCP,UpdatedSeries,var.est.t)
    mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))
    
    if (selectionF==TRUE){
        auxiliar_data <- UpdatedSeries
        auxiliar_data$signal <- predicted_signal-mean.est.t
        period <- periodic_estimation_selb_init(auxiliar_data,var.est.t,lyear)
        funct <- period$predict
        coeff <- period$coeff
    }
  } else {
    funct <- FALSE
    coeff <- FALSE
    Tmu <- FormatOptSegK(UpdatedCP,UpdatedSeries,var.est.t)
    mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))
    predicted_signal = mean.est.t
  }
  
  # compute SSR
  SSR <- sum(((UpdatedSeries$signal-predicted_signal)^2)/var.est.t,na.rm=TRUE)

  UpdatedPara <- c()
  UpdatedPara$MonthVar <- MonthVar
  UpdatedPara$Tmu   <-  Tmu
  UpdatedPara$FitF  <-  funct
  UpdatedPara$CoeffF <-  coeff
  UpdatedPara$SSR <- SSR
  return(UpdatedPara)

}

# This function estimates the coefficient of the periodic function at the same time as the segment means, for fixed CP positions
periodic_estimation_all = function(Data,CP,var.est.t,lyear){
  DataF=Data
  t=c(as.numeric(DataF$date))
  t0=c(as.numeric(DataF$date[1]))

  p <- 4
  for (i in 1:p){
    cosX=cos(i*(t-t0)*(2*pi)/lyear)
    sinX=sin(i*(t-t0)*(2*pi)/lyear)
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
  
  # prep data for regression with lm()
  DataF <- DataF %>%
    select(signal,cos1, sin1, cos2, sin2, cos3, sin3, cos4, sin4)
  DataF <- cbind(DataF, new_columns)
  # print(DataF)
  
  # regression
  reg=stats::lm(signal~-1+.,weights=1/var.est.t,data=DataF)
  coeff_all <- base::summary(reg)$coefficients[,1]

  # coeff of functional part
  coeff <- coeff_all[1:(2*p)]
  
  # compute functional part
  f <- rowSums(sapply(1:p, function(i) coeff[2*i-1]*cos(i*(t-t0+1)*(2*pi)/lyear) + coeff[2*i]*sin(i*(t-t0+1)*(2*pi)/lyear)))

  result=list()
  result$predict=stats::predict(reg,DataF)
  result$coeff_all=coeff_all
  result$coeff=coeff
  result$FitF=f
  return(result)
}
