#'Segmentation of time series
#'
#' fit a segmentation in the mean model by taken into account a functional part and a monthly variance
#'
#' @param OneSeries a data frame, with size n x 2, containing the signal with n points and the dates in Date format. The names of the 2 columns are thus signal and date
#' @param lmin the minimum length of the segments. Default is 1
#' @param Kmax the maximal number of segments (must be lower than n). Default is 30
#' @param selectionK a name indicating the model selection criterion to select the number of segments K (\code{mBIC}, \code{Lav}, \code{BM_BJ} or \code{BM_slope}). \code{"none"} indicates that no selection is claimed and the procedure considers \code{Kmax} segments or \code{Kmax}-1 changes. Default is \code{"BM_BJ"}
#' @param FunctPart a boolean indicating if the functional part is taking into account in the model. Default is TRUE and note that if \code{FunctPart=FALSE}, only a segmentation is performed
#' @param selectionF a boolean indicating if a selection on the functions of the Fourier decomposition of order 4 is performed. The level of the test is by default 0.001. Default is FALSE
#'
#' @return A file containing
#' \itemize{
#' \item \code{Tmu} that is a data frame containing the estimation of the segmentation parameters: for each segment, we get the beginning and the end positions (time index), the estimated mean (mean), the standard deviation of the mean (se) and the number of "valid" points (np) i.e. non-NA values in the segment
#' \item \code{FitF} that corresponds to the estimation of the functional part. If \code{FunctPart=FALSE}, \code{FitF} is FALSE
#' \item \code{CoeffF} that corresponds to the estimation of the coefficients of the Fourier decomposition. The vector contains 8 coefficients if \code{selectionF=FALSE} or as many coefficients as the number of selected functions if \code{selectionF=TRUE}. If \code{FunctPart=FALSE}, \code{CoeffF} is FALSE
#' \item \code{MonthVar} that corresponds to the estimated variances of each fixed interval (each month)
#' \item \code{SSR} that corresponds to the Residuals Sum of Squares for \code{Kmax} segments
#' }
#'
#' @details
#' The function performs the segmentation of one GNSS series. The considered model is such that: (1) the average is composed of a piece wise function (changes in the mean) with a functional part and (2) the variance is heterogeneous on fixed intervals. By default the latter intervals are the months.
#' The inference procedure consists in two steps. First, the number of segments is fixed to \code{Kmax} and the parameters are estimated using the maximum likelihood procedure using the following procedure: first the variances are robustly estimated and then the segmentation and the functional parts are iteratively estimated. Then the number of segments is chosen using model selection criteria. The possible criteria are \code{mBIC} the modified BIC criterion, \code{Lav} the criterion proposed by Lavielle, \code{BM_BJ} and \code{BM_slope} the criteriain which the penalty constant is calibrated using the Biggest Jump and the slope.
#' \itemize{
#' \item The data is a data frame with 2 columns: $signal is the signal to be segmented and $date is the date. The date will be in Date format.
#' \item The function part is estimated using a Fourier decomposition of order 4 with \code{selectionF=FALSE}. \code{selectionF=TRUE} consists in selecting the significant functions of the Fourier decomposition of order 4
#' \item If \code{selectionK="none"}, the procedure is performed with \code{Kmax} segments.
#' \item Missing data in the signal are accepted.
#' }
#'
#' @export


Segmentation <- function(OneSeries,lmin=1,Kmax=30,selectionK="BM_BJ",FunctPart=TRUE,selectionF=FALSE){
  result  <-  list()
  OneSeries.X  <-  c()
  cond1 <- TRUE
  cond2 <- TRUE
  cond3 <- TRUE

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

  if ((cond1==TRUE) & (cond2==TRUE) & (cond3==TRUE)){
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
  k=1
  kv=c()
  dv=c()
  pv=c()
  dmax=1
  while (k<Kmax) {
    pk=(J[(k+1):Kmax]-J[k])/(pen[k]-pen[(k+1):Kmax])
    pm=max(pk)
    dm=which.max(pk)
    dv=c(dv,dm)
    kv=c(kv,k)
    pv=c(pv,pm)
    if (dm>dmax){
      dmax=dm
      kmax=k
      pmax=pm
    } #end
    k=k+dm
  } #end

  pv=c(pv,0)
  kv=c(kv,Kmax)
  dv=diff(kv);
  dmax=max(dv)
  rt=which.max(dv)
  pmax=pv[rt[length(rt)]]
  alpha=2*pmax
  km=kv[alpha>=pv]
  km=km[1]
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
  Tmu = data.frame(rupt,mean.est.k, 1/sqrt(var.est.k),diff(c(0,breakpointsK)))
  colnames(Tmu) = c("begin","end","mean","se","np")
  return(Tmu)
}


###################################
######## Functions for functional
periodic_estimation_tot=function(Data,var.est.t,lyear){
  DataF=Data
  DataF$t=c(as.numeric(DataF$date-DataF$date[1]))+1
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
  DataF$t=c(as.numeric(DataF$date-DataF$date[1]))+1
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
  DataF$t=c(as.numeric(DataF$date-DataF$date[1]))+1
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
  DataF$t=c(as.numeric(DataF$date-DataF$date[1]))+1
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

