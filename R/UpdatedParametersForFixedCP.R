#' update the segmentation parameters after the screening or the test
#'
#' @param OneSeriesUpd is a time series data frame with 2 columns, $signal and $date, each of size n x 1, updated from the screening
#' @param UpdatedCP a vector of change-points remaining after screening or test.
#' @param FunctPart a boolean indicating if the functional part should be modelled. Default is TRUE.
#' @param selectionF a boolean indicating if the statistically significant parameters of the functional part should be selected. The level of the test is by default 0.001. Default is FALSE.
#' @param VarMonthly indicates if the variance is assumed to be monthly (\code{VarMonthly=TRUE}) or homogeneous (\code{VarMonthly=FALSE}). Default is \code{TRUE}.
#'
#' @return
#' \itemize{
#' \item \code{Tmu} is a data frame containing the segmentation results, with 5 columns and a number of lines equal to the number of segments of the time series.
#'   The columns are: \code{$begin, $end, $tbegin, $tend, $mean, $se, $np}. They represent the date index (integer) of begin and end of each segment, their corresponding dates (tbegin and tend), the estimated mean of the segment (\code{mean}) and its standard error (\code{se}), and the number of "valid" points (\code{np}), i.e. non-NA $signal values in the segment.
#' \item \code{FitF} is the functional part predicted from the estimated Fourier coefficients, a numeric vector of size n x 1. Note: if \code{FunctPart=FALSE}, \code{FitF} is FALSE.
#' \item \code{CoeffF} is the vector of coefficients of the Fourier series, a numeric vector of size 1 x 8 if \code{selectionF=FALSE}.
#'   Note: If \code{selectionF=TRUE} the size of \code{CoeffF} corresponds to the number of selected coefficients.
#'         If \code{FunctPart=FALSE}, \code{CoeffF} is FALSE.
#' \item \code{MonthVar} contains the estimated monthly variances, a numeric vector of size 1 x 12 if \code{VarMonthly=TRUE}, one value otherwise.
#' \item \code{SSR} is the Sum of Squared Residuals of the fit.
#' }
#'
#' @export

UpdatedParametersForFixedCP <- function(OneSeriesUpd, UpdatedCP, FunctPart=TRUE, selectionF=FALSE, VarMonthly=TRUE){

  UpdatedPara <- c()

  # Estimate monthly variance parameters
   OneSeriesUpd$year  <- as.factor(format(OneSeriesUpd$date,format='%Y'))
   OneSeriesUpd$month <- as.factor(format(OneSeriesUpd$date,format='%m'))
   OneSeriesUpd$month <-  droplevels(OneSeriesUpd$month)
   OneSeriesUpd$year  <-  droplevels(OneSeriesUpd$year)

  # Estimate the variance parameters and compute the variance time series
  if (VarMonthly==TRUE){
    # Estimate all Montly variances
    sigma.est.month <- RobEstiMonthlyVariance(OneSeriesUpd)
    MonthVar <- sigma.est.month^2
    # Compute the variance time series
    var.est.t <-  MonthVar[as.numeric(OneSeriesUpd$month)]
  } else {
    # Estimate one single variance
    Y_diff <- diff(OneSeriesUpd$signal[which(!is.na(OneSeriesUpd$signal))])
    sigma.est.month <- robustbase::Qn(Y_diff)/sqrt(2)
    MonthVar <- sigma.est.month^2
    # Compute the variance time series
    var.est.t <- rep(MonthVar,length(OneSeriesUpd$date))
  }
  var.est.t[which(is.na(OneSeriesUpd$signal))] <- NA


  # Update CPs
  UpdatedCP = which(OneSeriesUpd$date %in% OneSeriesUpd$date[UpdatedCP])
  UpdatedCP <- c(UpdatedCP, nrow(OneSeriesUpd))
  # print(UpdatedCP)

  # Re-estimate means of segments and functional part
  if (FunctPart==TRUE){
    lyear <- 365.25
    CoeffF <- c()
    FitF <- c()
    predicted_signal <- c()

    UpdatedSignalModel <- periodic_estimation_all(Data = OneSeriesUpd, CP = UpdatedCP, var.est.t = var.est.t, lyear = lyear)

    predicted_signal <- UpdatedSignalModel$predict
    CoeffF <- UpdatedSignalModel$CoeffF
    FitF <- UpdatedSignalModel$FitF

    Tmu <- FormatOptSegK(UpdatedCP,OneSeriesUpd,var.est.t)
    mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))

    if (selectionF==TRUE){
        auxiliar_data <- OneSeriesUpd
        auxiliar_data$signal <- predicted_signal-mean.est.t
        period <- periodic_estimation_selb_init(auxiliar_data,var.est.t,lyear)
        FitF <- period$predict
        CoeffF <- period$CoeffF
    }
  } else {
    FitF <- FALSE
    CoeffF <- FALSE
    Tmu <- FormatOptSegK(UpdatedCP,OneSeriesUpd,var.est.t)
    mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))
    predicted_signal = mean.est.t
  }

  # compute SSR
  SSR <- sum(((OneSeriesUpd$signal-predicted_signal)^2)/var.est.t,na.rm=TRUE)
  UpdatedPara$MonthVar <- MonthVar
  UpdatedPara$Tmu   <- Tmu
  UpdatedPara$FitF  <- FitF
  UpdatedPara$CoeffF <- CoeffF
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
  #result$coeff_all=coeff_all
  result$CoeffF=coeff
  result$FitF=f
  return(result)
}
