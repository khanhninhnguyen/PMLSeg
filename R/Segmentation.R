#' Segmentation of time series by Penalized Maximum Likelihood
#'
#' Fit a segmentation model comprising one mean per segment, a global functional (Fourier series of order 4), and IID noise with variance changing over fixed intervals. 
#' The time series should be given with a daily resolution, the functional has a fundamental period of 1 year, and the variances are estimated on monthly intervals.
#'
#' @param OneSeries is a time series data frame with 2 columns, $signal and $date, each of size n x 1, n is the number of days of the time series.
#'    Note: the $date variable should be continous. If the original data has gaps, NAs should be added at the corresponding dates.
#' @param lmin is the minimum length of the segments. Default value is 1.
#' @param Kmax is the maximal number of segments (Kmax < n). Default value is 30. Note: with \code{BM_slope}, \code{Kmax} must be larger than or equal to 10.
#' @param selectionK specifies the penalty criterion used for the model selection (selection of the number of segments K <= Kmax). Options are: \code{"none"}, \code{mBIC}, \code{Lav}, \code{BM_BJ} or \code{BM_slope}). 
#' If \code{selectionK = "none"}, the model is estimated with \code{K = Kmax}. Default is \code{"BM_BJ"}.
#' @param FunctPart specifies if the functional part (Fourier series of order 4) should be included in the model (\code{FunctPart=TRUE}) or not (\code{FunctPart=FALSE}). Default is TRUE. 
#'   Note: with \code{FunctPart=TRUE} the algorithm estimates the functional and the segmentation parameters in an iterative way; with \code{FunctPart=FALSE}, only one segmentation is performed.
#'   If the functional part is unnecessary, \code{FunctPart=FALSE} can be much faster.
#' @param selectionF is used to select only significant coefficients of the Fourier series when \code{FunctPart=TRUE}. Default is FALSE.
#'
#' @return 
#' \itemize{
#' \item \code{Tmu} is a data frame containing the segmentation results, with 5 columns and a number of lines equal to the number of segments of the time series. 
#'    The columns are: \code{$begin, $end, $mean, $se, $np}. They represent the date index (integer) of begin and end of each segment, the estimated mean of the segment (\code{mean}) and its standard error (\code{se}), and the number of "valid" points (\code{np}), i.e. non-NA $signal values in the segment.
#' \item \code{FitF} is the functional part predicted from the estimated Fourier coefficients, a numeric vector of size n x 1. Note: if \code{FunctPart=FALSE}, \code{FitF} is FALSE.
#' \item \code{CoeffF} is the vector of coefficients of the Fourier series, a numeric vector of size 1 x 8 if \code{selectionF=FALSE}.
#'    Note: If \code{selectionF=TRUE} the size of \code{CoeffF} correspods to the number of selected coefficients. If \code{FunctPart=FALSE}, \code{CoeffF} is FALSE.
#' \item \code{MonthVar} contains the estimated monthly variances, a numeric vector of size 1 x 12.
#' \item \code{SSR} is the Sum of Squared Residuals of the fit.
#' }
#'
#' @details
#' The theoretical basis of the segmentation method developped by Quarello (2020) and published in Quarello et al., (2022). 
#' The inference procedure consists in three steps:
#' \enumerate{
#' \item The monthly variances are estimated using a robust method following Bock et al. (2020).
#' \item The segmentation parameters (change-point positions and segments' means) and the functional parameters (Fourier coefficients) are estimated iteratively, for a fixed number of segments \code{K = 1..Kmax}.
#' The estimation method is based on maximum likelihood with known variance (from step 1). 
#' The segmentation and functional parameters are estimated separately, which allows to use the Dynamical Programming algorithm for the search of the optimal change-point positions and segment means.
#' \item The optimal number of segments is obtained by model selection using one of the following penalty criteria:
#'   \itemize{
#'   \item \code{mBIC}, the modified Bayesian Inference Criterion proposed by Zhang and Siegmund (2007),
#'   \item \code{Lav}, the criterion proposed Lavielle (2005),
#'   \item \code{BM_BJ} and \code{BM_slope}, the criteria proposed by Birgé and Massart (2001), where the penalty constant is calibrated using the Biggest Jump and the slope, respectively.
#'   }
#' }
#'
#' Note: by convention, the position of a change-point refers to the last point in a segment (\code{Tmu$end}).
#'
#' @references
#' Birgé, L.; Massart, P. (2001) Gaussian model selection. J. Eur. Math. Soc. 2001, 3, 203–268, DOI 10.1007/S100970100031.
#'
#' Bock, O.; Collilieux, X.; Guillamon, F.; Lebarbier, E.; Pascal, C. (2020) A breakpoint detection in the mean model with heterogeneous variance on fixed time intervals. Statistics and Computing 2020, 30, 195–207. https://doi.org/10.1007/s11222-019-09853-5.
#'
#' Lavielle, M. (2005) Using penalized contrasts for the change-point problem. Signal Processing 2005, 85, 1501–1510.
#'
#' Quarello, A. (2020) A. Development of New Homogenisation Methods for GNSS Atmospheric Data. Application to the Analysis of Climate Trends and Variability. Ph.D. Thesis, Sorbonne Universite, Paris, France, 2020
#'
#' Quarello, A., Bock, O. & Lebarbier, E. (2022) GNSSseg, a statistical method for the segmentation of daily GNSS IWV time series. Remote Sensing, 14(14), 3379. Available from: https://doi.org/10.3390/rs14143379
#'
#' Zhang, N.R.; Siegmund, D.O. (2007) A Modified Bayes Information Criterion with Applications to the Analysis of Comparative Genomic Hybridization Data. Biometrics 2007, 63, 22–32.
#'
#' @export

Segmentation <- function(OneSeries,lmin=1,Kmax=30,selectionK="BM_BJ",FunctPart=TRUE,selectionF=FALSE){
  result  <-  list()
  OneSeries.X  <-  c()
  cond1 <- TRUE
  cond2 <- TRUE
  cond3 <- TRUE
  cond4 <- TRUE
  #Parameters
  lyear <- 365.25
  S     <- 0.75
  threshold<- 0.001
  tol   <-  1e-4

  #For NA
  present.data  <-  which(!is.na(OneSeries$signal))
  OneSeries.X   <-  OneSeries[present.data,]
  n.OneSeries   <- length(OneSeries$signal)
  n.X           <- length(OneSeries.X$signal)
  Kseq          <- 1:Kmax


  #The conditions to be fulfilled
  if (!inherits(OneSeries$date, "Date")){
    cond1 <- FALSE
    warning("date must be in Date format")
  }


  if (Kmax >n.X) {
    cond2 <- FALSE
    warning("The maximal number of segments Kmax ", Kmax," needs to be lower than the length of the series without NA that is " ,n.X,"\n")
  }

  if(lmin > round(n.X/Kmax)){
    cond3 <- FALSE
    warning("The minimal length of the segments lmin", lmin," needs to be lower than " ,round(n.X/Kmax),"\n")
  }

  if(Kmax==1){
    cond4 <- FALSE
    warning("Kmax=1 means no segmentation")
  }

  if ((cond1==TRUE) & (cond2==TRUE) & (cond3==TRUE) & (cond4==TRUE)){
    OneSeries.X$year <- as.factor(format(OneSeries.X$date,format='%Y'))
    OneSeries.X$month <- as.factor(format(OneSeries.X$date,format='%m'))
    OneSeries.X$month <-  droplevels(OneSeries.X$month)
    OneSeries.X$year   <-  droplevels(OneSeries.X$year)


    #Used function for NA
    add_NA=function(res,present.data,n.OneSeries,segf){
      res.with.NA<-list()
      #Segmentation
      Tmu.temp<-res$Tmu
      Tmu.temp$begin<-present.data[Tmu.temp$begin]
      Tmu.temp$end<-present.data[Tmu.temp$end]
      Tmu.temp$end[length(Tmu.temp$end)]<-n.OneSeries
      #Function
      if (segf==TRUE){
        f.temp<-rep(NA,n.OneSeries)
        f.temp[present.data]<-res$f
        res.with.NA$f<-f.temp
      } else {f.temp<-FALSE}
      res.with.NA$Tmu<-Tmu.temp
      return(res.with.NA)
    }


    #Estimation of the Montly variances
    sigma.est.month<-RobEstiMonthlyVariance(OneSeries.X)
    var.est.month<-sigma.est.month^2

    #Creation of graphs
    myGraph <- GraphBuilding(lmin,Kmax)

    if (FunctPart==TRUE){
      #Option for estimating f
      Used.function=c()
      if (selectionF==TRUE){
        Used.function<-'Seg_funct_selbK'
      } else{
        Used.function<-'Seg_funct_totK'
      }


      if (selectionK=="none"){
        res.segfunct <- c()
        request <- paste(paste0("res.segfunct=",Used.function,'(OneSeries.X,var.est.month,Kmax,myGraph[[Kmax]],lmin,lyear,threshold,tol)'),sep="")
        eval(parse(text=request))
        res.segfunct.with.NA <-  add_NA(res.segfunct,present.data,n.OneSeries,segf=TRUE)
        Tmu <- res.segfunct.with.NA$Tmu
        funct <- res.segfunct.with.NA$f
        coeff <- res.segfunct$coeff
        SSwg <- res.segfunct$SSwg
        Kh <- Kmax
      }

      if (selectionK=="Lav"){
        res.LoopK <- Loop.K.procedure(OneSeries.X,var.est.month,lyear,lmin,Kmax,myGraph,Used.function,threshold,tol)
        res <- sapply(res.LoopK,function(e) {
          return(c(SSwg =e$SSwg, LogLg = e$LogLg))
        })
        SSwg<-res[1,]
        Kh <- MLcriterion(SSwg, Kseq,S)
        res.segfunct<-res.LoopK[[Kh]]
        res.segfunct.with.NA<- add_NA(res.segfunct,present.data,n.OneSeries,segf=TRUE)
        Tmu <- res.segfunct.with.NA$Tmu
        funct<-res.segfunct.with.NA$f
        coeff<-res.segfunct$coeff
      }

      if (selectionK=="BM_BJ"){
        res.LoopK <- Loop.K.procedure(OneSeries.X,var.est.month,lyear,lmin,Kmax,myGraph,Used.function,threshold,tol)
        res <- sapply(res.LoopK,function(e) {
          return(c(SSwg =e$SSwg, LogLg = e$LogLg))
        })
        SSwg <- res[1,]
        pen <- 5*Kseq+2*Kseq*log(n.X/Kseq)
        Kh <- BMcriterion(SSwg,pen)
        res.segfunct <- res.LoopK[[Kh]]
        res.segfunct.with.NA<- add_NA(res.segfunct,present.data,n.OneSeries,segf=TRUE)
        Tmu <- res.segfunct.with.NA$Tmu
        funct <- res.segfunct.with.NA$f
        coeff <- res.segfunct$coeff
      }

      if (selectionK=="BM_slope"){
        res.LoopK<-Loop.K.procedure(OneSeries.X,var.est.month,lyear,lmin,Kmax,myGraph,Used.function,threshold,tol)
        res<-sapply(res.LoopK,function(e) {
          return(c(SSwg =e$SSwg, LogLg = e$LogLg))
        })
        SSwg <- res[1,]
        pen<-5*Kseq+2*Kseq*log(n.X/Kseq)
        DataForCa<-data.frame(model=paste("K=",Kseq),pen=pen,complexity=Kseq,contrast=SSwg)
        Kh<-Kseq[which(capushe::DDSE(DataForCa)@model==DataForCa$model)]
        res.segfunct<-res.LoopK[[Kh]]
        res.segfunct.with.NA<- add_NA(res.segfunct,present.data,n.OneSeries,segf=TRUE)
        Tmu<-res.segfunct.with.NA$Tmu
        funct<-res.segfunct.with.NA$f
        coeff<-res.segfunct$coeff
      }

      if (selectionK=="mBIC"){
        res.LoopK <- Loop.K.procedure(OneSeries.X,var.est.month,lyear,lmin,Kmax,myGraph,Used.function,threshold,tol)
        res <- sapply(res.LoopK,function(e) {
          return(c(SSwg =e$SSwg, LogLg = e$LogLg))
        })
        SSwg<-res[1,]
        LogLg <- res[2,]
        Kh <- mBICcriterion(SSwg,LogLg,n.X,Kseq)$Kh
        res.segfunct <- res.LoopK[[Kh]]
        res.segfunct.with.NA <- add_NA(res.segfunct,present.data,n.OneSeries,segf=TRUE)
        Tmu <- res.segfunct.with.NA$Tmu
        funct <- res.segfunct.with.NA$f
        coeff <- res.segfunct$coeff
      }

    } else {
      funct<-FALSE
      coeff<-FALSE

      res.LoopK<-Loop.K.seg.procedure(OneSeries.X,var.est.month,lyear,lmin,Kmax,myGraph)
      res.seg<-sapply(res.LoopK,function(e) {
        return(c(SSwg =e$SSwg, LogLg = e$LogLg))
      })
      SSwg<-res.seg[1,]
      LogLg<-res.seg[2,]
      pen<-5*Kseq+2*Kseq*log(n.X/Kseq)

      if (selectionK=="none"){
        res.seg.with.NA <- add_NA(res.LoopK[[Kmax]],present.data,n.OneSeries,segf=FALSE)
        Tmu <- res.seg.with.NA$Tmu
        Kh <- Kmax
      }

      if (selectionK=="Lav"){
        Kh <- MLcriterion(SSwg, Kseq,S)
        res.seg.sol <- c()
        res.seg.sol <- res.LoopK[[Kh]]
        res.seg.with.NA <- add_NA(res.seg.sol,present.data,n.OneSeries,segf=FALSE)
        Tmu <- res.seg.with.NA$Tmu
      }

      if (selectionK=="BM_BJ"){
        Kh <- BMcriterion(SSwg,pen)
        res.seg.sol <- c()
        res.seg.sol <- res.LoopK[[Kh]]
        res.seg.with.NA <- add_NA(res.seg.sol,present.data,n.OneSeries,segf=FALSE)
        Tmu <- res.seg.with.NA$Tmu
      }

      if (selectionK=="BM_slope"){
        DataForCa <- data.frame(model=paste("K=",Kseq),pen=pen,complexity=Kseq,contrast=SSwg)
        Kh <- Kseq[which(capushe::DDSE(DataForCa)@model==DataForCa$model)]
        res.seg.sol <- c()
        res.seg.sol <- res.LoopK[[Kh]]
        res.seg.with.NA <- add_NA(res.seg.sol,present.data,n.OneSeries,segf=FALSE)
        Tmu <- res.seg.with.NA$Tmu
      }

      if (selectionK=="mBIC"){
        Kh <- mBICcriterion(SSwg,LogLg,n.X,Kseq)$Kh
        res.seg.sol <- c()
        res.seg.sol <- res.LoopK[[Kh]]
        res.seg.with.NA <- add_NA(res.seg.sol,present.data,n.OneSeries,segf=FALSE)
        Tmu <- res.seg.with.NA$Tmu
      }
    }

    #Obtained segmentation
    result$Tmu <- Tmu
    result$FitF <- funct
    result$CoeffF <- coeff
    #Global results
    result$MonthVar <- var.est.month

    if (length(SSwg)==1){
      result$SSR <- SSwg
    } else {result$SSR <- SSwg[Kh]}
    return(result)
  }

}


#################################
# Used functions
###################################

###################################
######## General Loop over K

Loop.K.procedure = function(Data,var.est.month,lyear,lmin,Kmax,myGraph,Used.function,threshold,tol){
  result  = list()
  n       = dim(Data)[1]
  #Iterative procedure
  result=lapply(1:Kmax, function(i){
    res.segfunct=c()
    request=paste(paste0("res.segfunct=",Used.function,'(Data,var.est.month,i,myGraph[[i]],lmin,lyear,threshold,tol)'),sep="")
    eval(parse(text=request))
  })
  return(result)
}

Loop.K.seg.procedure = function(Data,var.est.month,lyear,lmin,Kmax,myGraph){
  result  = list()
  n       = dim(Data)[1]
  var.est.t=var.est.month[as.numeric(Data$month)]
  result=lapply(1:Kmax, function(i){
    res.segfunct=c()
    request=paste(paste0("res.segfunct=SegMonthlyVarianceK(Data,i,myGraph[[i]],lmin,var.est.t)"),sep="")
    eval(parse(text=request))
  })
  return(result)
}

###################################
######## Model selection criteria

MLcriterion<-function(J,Kseq,S=0.75)
{
  Kmax=length(Kseq)
  Jtild=(J[Kmax]-J)/(J[Kmax]-J[1])*(Kseq[Kmax]-Kseq[1])+1
  D=diff(diff(Jtild))

  if (length(which(D>=S))>0){
    Kh <- max(which(D>=S))+1
  }else {
    Kh <- 1
  }
  Kh=Kseq[Kh]
  return(Kh)
}

BMcriterion<-function(J,pen)
{
  Kmax=length(J)
  Kseq=1:Kmax
  kv <- chull(pen, J) %>% 
  rev() %>% 
  { if (last(.) != Kmax) c(., Kmax) else . }
   pv <- -diff(J[kv])/diff(pen[kv])
  dv <- diff(kv)
  rg <- which(pv>0)
  pv <- pv[rg]
  dv <- dv[rg]

  dmax <- max(dv)  # Trouver le maximum de dv
  rt <- which.max(dv)  # Trouver l'indice du maximum

  pmax <- pv[rt]  # Extraire la valeur correspondante dans pv
  alpha <- 2 * pmax  # Calculer alpha

  km <- kv[alpha >= pv] %>% first()
  Kh =Kseq[km]
  return(Kh)
}

mBICcriterion <- function(SSwg,LogLg,n,Kseq){
  prova=list()
  Kh=c()
  mBIC=c()
  mBIC=(1/2)*(-SSwg)-(1/2)*LogLg+(3/2-Kseq)*log(n)
  Kh.mBIC=which.max(mBIC)
  prova$Kh=Kh.mBIC
  prova$mBIC=mBIC
  return(prova)
}



###################################
########  Inference procedure for a fixed K
Seg_funct_totK <-function(Data,var.est.month,K,graphK,lmin,lyear,threshold,tol){
  var.est.t <- var.est.month[as.numeric(Data$month)]

  period <- periodic_estimation_tot_init(Data,var.est.t,lyear)
  auxiliar_data <- Data
  auxiliar_data$signal <- Data$signal-period$predict
  segmentation <- SegMonthlyVarianceK(auxiliar_data,K,graphK,lmin,var.est.t)

  maxIter = 100
  Diff    = 2*tol
  Iter=0

  while ((Diff  > tol) & (Iter < maxIter))
  {
    Iter = Iter +1
    auxiliar_data$signal=Data$signal-segmentation$mean.est.t
    periodi=periodic_estimation_tot(auxiliar_data,var.est.t,lyear)

    auxiliar_data$signal=Data$signal-periodi$predict
    segmi=SegMonthlyVarianceK(auxiliar_data,K,graphK,lmin,var.est.t)

    if (Iter == 2)
    {
      t2 = c(period$predict,segmentation$mean.est.t)
    }
    if (Iter == 3)
    {
      t1 = c(period$predict,segmentation$mean.est.t)
      t0 = c(periodi$predict,segmi$mean.est.t)
      tp0 = (t2-t1)/sum((t1==t2)+((t2-t1)^2)) + (t0-t1)/sum((t1==t0)+((t0-t1)^2))
      tp0 = t1 + tp0 / sum((tp0==0) + tp0^2)
    }
    if (Iter > 3)
    {
      t2 = t1
      t1 = t0
      t0 = c(periodi$predict,segmi$mean.est.t)
      tp1 = tp0
      tp0 = (t2-t1)/sum((t1==t2)+((t2-t1)^2)) + (t0-t1)/sum((t1==t0)+((t0-t1)^2))
      tp0 = t1 + tp0 / sum((tp0==0) + tp0^2)
      Diff = sum((tp0-tp1)^2)
    }

    period=periodi
    segmentation=segmi
  }


  segmK=c()
  segmK$Tmu   = segmentation$Tmu
  segmK$SSwg  = segmentation$SSwg
  segmK$LogLg = segmentation$LogLg
  segmK$f     = period$predict
  segmK$coeff = period$coeff
  return(segmK)

}


Seg_funct_selbK <-function(Data,var.est.month,K,graphK,lmin,lyear,threshold,tol){
  var.est.t=var.est.month[as.numeric(Data$month)]

  period=periodic_estimation_selb_init(Data,var.est.t,lyear,threshold)
  auxiliar_data <- Data
  auxiliar_data$signal=Data$signal-period$predict
  segmentation=SegMonthlyVarianceK(auxiliar_data,K,graphK,lmin,var.est.t)

  maxIter = 100
  Diff  = 2*tol
  Iter  = 0

  while ((Diff  > tol) & (Iter < maxIter))
  {
    Iter = Iter +1
    auxiliar_data$signal=Data$signal-segmentation$mean.est.t
    periodi=periodic_estimation_selb(auxiliar_data,var.est.t,lyear,threshold)

    auxiliar_data$signal=Data$signal-periodi$predict
    segmi=SegMonthlyVarianceK(auxiliar_data,K,graphK,lmin,var.est.t)

    if (Iter == 2)
    {
      t2 = c(period$predict,segmentation$mean.est.t)
    }
    if (Iter == 3)
    {
      t1 = c(period$predict,segmentation$mean.est.t)
      t0 = c(periodi$predict,segmi$mean.est.t)
      tp0 = (t2-t1)/sum((t1==t2)+((t2-t1)^2)) + (t0-t1)/sum((t1==t0)+((t0-t1)^2))
      tp0 = t1 + tp0 / sum((tp0==0) + tp0^2)
    }
    if (Iter > 3)
    {
      t2 = t1
      t1 = t0
      t0 = c(periodi$predict,segmi$mean.est.t)
      tp1 = tp0
      tp0 = (t2-t1)/sum((t1==t2)+((t2-t1)^2)) + (t0-t1)/sum((t1==t0)+((t0-t1)^2))
      tp0 = t1 + tp0 / sum((tp0==0) + tp0^2)
      Diff = sum((tp0-tp1)^2)
    }

    period=periodi
    segmentation=segmi

  }


  segmK=c()
  segmK$Tmu   = segmentation$Tmu
  segmK$SSwg  = segmentation$SSwg
  segmK$LogLg = segmentation$LogLg
  segmK$f     = period$predict
  segmK$coeff = period$coeff
  #segmK$
  return(segmK)

}


###################################
######## Robust estimation of the variances

RobEstiMonthlyVariance <- function(Y){
  Kmonth <- length(unique(Y$month))
  z <- stats::aggregate(signal ~ month + year, data = Y, diff)
  sigma.est <- sapply(1:Kmonth,function(i) {
    e <- subset(z,z$month==levels(z$month)[i])
    ee <- unlist(e$signal)
    robustbase::Qn(ee)/sqrt(2)
  })
  return(sigma.est)
}




###################################
######## Functions for segmentation

SegMonthlyVarianceK=function(Data,K,graphK,lmin,var.est.t){
  result <- list()
  Res.gfpop <- c()
  Res.gfpop <- gfpop::gfpop(data = Data$signal, mygraph = graphK, type = "mean",weights=1/var.est.t)


  Tmu <- c()
  Tmu <- FormatOptSegK(Res.gfpop$changepoints,Data,var.est.t)
  mean.est.t  = rep(Tmu$mean,diff(c(0,Tmu$end)))

  result$Tmu=Tmu
  result$mean.est.t = mean.est.t
  result$SSwg=Res.gfpop$globalCost
  result$LogLg=sum(log(diff(c(0,Res.gfpop$changepoints))[diff(c(0,Res.gfpop$changepoints))>0]))
  return(result)

}

###################################
######## Format of the segmentation result

FormatOptSegK <- function(breakpointsK,Data,v){
  K     = length(breakpointsK)
  rupt  = matrix(Inf,ncol = 2 , nrow= K)
  bp    = breakpointsK
  rupt[,2]  = bp
  bp        = bp +1
  rupt[,1]  = c(1, bp[1:K-1])

  Data.var   = Data$signal/v
  mean.est.k = apply(rupt,1,FUN=function(z) sum(Data.var[z[1]:z[2]],na.rm=TRUE))
  var.est.k = apply(rupt,1,FUN=function(z) sum(1/(v[z[1]:z[2]]),na.rm=TRUE))
  mean.est.k = mean.est.k/var.est.k
  np.k      = apply(rupt,1,FUN=function(z) sum(!is.na(Data$signal[z[1]:z[2]])))
  Tmu = data.frame(rupt,mean.est.k, 1/sqrt(var.est.k),np.k)
  colnames(Tmu) = c("begin","end","mean","se","np")
  return(Tmu)
}


###################################
######## Functions for functional
periodic_estimation_tot=function(Data,var.est.t,lyear){
  DataF=Data
  DataF$t=c(as.numeric(DataF$date-DataF$date[1]))
  for (i in 1:4){
    cosX=cos(i*DataF$t*(2*pi)/lyear)
    sinX=sin(i*DataF$t*(2*pi)/lyear)
    DataF=cbind(DataF,cosX,sinX)
    colnames(DataF)[(dim(DataF)[2]-1):(dim(DataF)[2])]=c(paste0('cos',i),paste0('sin',i))
  }
  reg=stats::lm(signal~-1+cos1+sin1+cos2+sin2+cos3+sin3+cos4+sin4,weights=1/var.est.t,data=DataF)
  coeff=base::summary(reg)$coefficients[,1]
  result=list()
  result$predict=stats::predict(reg,DataF)
  result$coeff=coeff
  return(result)
}


periodic_estimation_tot_init=function(Data,var.est.t,lyear){
  DataF=Data
  DataF$t=c(as.numeric(DataF$date-DataF$date[1]))
  for (i in 1:4){
    cosX=cos(i*DataF$t*(2*pi)/lyear)
    sinX=sin(i*DataF$t*(2*pi)/lyear)
    DataF=cbind(DataF,cosX,sinX)
    colnames(DataF)[(dim(DataF)[2]-1):(dim(DataF)[2])]=c(paste0('cos',i),paste0('sin',i))
  }
  reg=stats::lm(signal~-1+cos1+sin1+cos2+sin2+cos3+sin3+cos4+sin4,data=DataF)
  coeff=base::summary(reg)$coefficients[,1]
  result=list()
  result$predict=stats::predict(reg,DataF)
  result$coeff=coeff
  return(result)
}


periodic_estimation_selb=function(Data,var.est.t,lyear,threshold=0.001){
  DataF=Data
  DataF$t=c(as.numeric(DataF$date-DataF$date[1]))
  num.col=dim(DataF)[2]
  for (i in 1:4){
    cosX=cos(i*DataF$t*(2*pi)/lyear)
    sinX=sin(i*DataF$t*(2*pi)/lyear)
    DataF=cbind(DataF,cosX,sinX)
    colnames(DataF)[(dim(DataF)[2]-1):(dim(DataF)[2])]=c(paste0('cos',i),paste0('sin',i))
  }
  reg=stats::lm(signal~-1+cos1+sin1+cos2+sin2+cos3+sin3+cos4+sin4,weights=1/var.est.t,data=DataF)
  res.coeff=summary(reg)$coefficients
  names.coeff=rownames(res.coeff)
  #Selection of significant coefficients
  rg=which(res.coeff[,4]<threshold)

  if (length(rg)>=1){
    names.Selected=names.coeff[rg]
    n.Selected=length(names.Selected)
    DataFF=DataF[,c(1:num.col,which(colnames(DataF) %in%  names.Selected ))]
    if (n.Selected >=2){
      a=names.Selected[1]
      for (i in 1:(n.Selected-1)){
        a=paste(a,names.Selected[i+1],sep="+")
      }
    } else {
      a=names.Selected[1]
    }
    reg.Selected=c()
    request=paste(paste0("reg.Selected=stats::lm(signal~-1+",a,",weights=1/var.est.t,data=DataFF)"),sep="")
    eval(parse(text=request))
    pred=reg.Selected$fitted.values
    coeff=reg.Selected$coefficients
  } else {
    pred=rep(0,length(DataF$signal))
    coeff=0
    names(coeff)="no selected coeff"
  }
  result=list()
  result$predict=pred
  result$coeff=coeff
  return(result)
}



periodic_estimation_selb_init=function(Data,var.est.t,lyear,threshold=0.001){
  DataF=Data
  DataF$t=c(as.numeric(DataF$date-DataF$date[1]))
  num.col=dim(DataF)[2]
  for (i in 1:4){
    cosX=cos(i*DataF$t*(2*pi)/lyear)
    sinX=sin(i*DataF$t*(2*pi)/lyear)
    DataF=cbind(DataF,cosX,sinX)
    colnames(DataF)[(dim(DataF)[2]-1):(dim(DataF)[2])]=c(paste0('cos',i),paste0('sin',i))
  }


  reg=stats::lm(signal~-1+cos1+sin1+cos2+sin2+cos3+sin3+cos4+sin4,data=DataF)
  res.coeff=summary(reg)$coefficients
  names.coeff=rownames(res.coeff)
  #Selection of significant coefficients
  rg=which(res.coeff[,4]<threshold)

  if (length(rg)>=1){
    names.Selected=names.coeff[rg]
    n.Selected=length(names.Selected)
    DataFF=DataF[,c(1:num.col,which(colnames(DataF) %in%  names.Selected ))]
    if (n.Selected >=2){
      a=names.Selected[1]
      for (i in 1:(n.Selected-1)){
        a=paste(a,names.Selected[i+1],sep="+")
      }
    } else {
      a=names.Selected[1]
    }
    reg.Selected=c()
    request=paste(paste0("reg.Selected=stats::lm(signal~-1+",a,",data=DataFF)"),sep="")
    eval(parse(text=request))
    pred=reg.Selected$fitted.values
    coeff=reg.Selected$coefficients
  } else {
    pred=rep(0,length(DataF$signal))
    coeff=0
    names(coeff)="no selected coeff"
  }
  result=list()
  result$predict=pred
  result$coeff=coeff
  return(result)
}

###################################
######## Construction of the graph for the gfpop

GraphBuilding<- function(lmin,Kmax){
  myGraph=list()
  if (lmin==1){
    myGraph[[1]] <- gfpop::graph(
      gfpop::Edge(0, 0, "null")
    )
    for (k in 2:Kmax){
      myGraph.add<-gfpop::graph(
        gfpop::Edge(k-2, k-1,"std"),
        gfpop::Edge(k-1, k-1, "null")
      )
      myGraph[[k]] <- rbind(myGraph[[k-1]],myGraph.add)
    }
  }

  if (lmin>1){
    myGraph=list()
    #k=1
    subgraph1 <- c()
    subgraph1 <-gfpop::graph(gfpop::Edge(0, paste0("wait",0,".",1,sep=""), "null"))
    subgraph2 <- gfpop::graph()
    if(lmin>2){
      for (l in 2:(lmin-1)){
        subgraph2 <- rbind(subgraph2,gfpop::graph(gfpop::Edge(paste0("wait",0,".",l-1,sep=""), paste0("wait",0,".",l,sep=""))))
      }
    }
    subgraph3 <-gfpop::graph()
    subgraph3 <- gfpop::graph(gfpop::Edge(paste0("wait",0,".",lmin-1,sep=""), paste0("wait",0,".",lmin-1,sep=""), "null"))
    myGraph[[1]] <-rbind(subgraph1,subgraph2,subgraph3)

    #for each k
    for (k in 2:Kmax){
      subgraph1 <- gfpop::graph()
      subgraph1 <- gfpop::graph(gfpop::Edge(myGraph[[k-1]]$state1[dim(myGraph[[k-1]])[1]], k-1, "std"),gfpop::Edge(k-1, paste0("wait",k-1,".",1,sep=""), "null"))
      subgraph2 <- gfpop::graph()
      if(lmin>2){
        for (l in 2:(lmin-1)){
          subgraph2 <- rbind(subgraph2,gfpop::graph(gfpop::Edge(paste0("wait",k-1,".",l-1,sep=""), paste0("wait",k-1,".",l,sep=""))))
        }
      }
      subgraph3 <-gfpop::graph()
      subgraph3 <- gfpop::graph(gfpop::Edge(paste0("wait",k-1,".",lmin-1,sep=""), paste0("wait",k-1,".",lmin-1,sep=""), "null"))

      myGraph[[k]] <- rbind(myGraph[[k-1]],subgraph1,subgraph2,subgraph3)
      myGraph[[k-1]]<-rbind(myGraph[[k-1]],gfpop::StartEnd(start = 0, end = myGraph[[k-1]]$state1[dim(myGraph[[k-1]])[1]]))
    }

    myGraph[[Kmax]]<-rbind(myGraph[[Kmax]],gfpop::StartEnd(start = 0, end = myGraph[[Kmax]]$state1[dim(myGraph[[Kmax]])[1]]))
  }
  return(myGraph)
}

