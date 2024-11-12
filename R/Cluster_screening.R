#' Post-processing, called screening, of the detected change-points
#'
#' (1) detection of cluster of change-points (formed by consecutive change-points separated by a maximum number of days)
#' (2) testing the variation of mean before and after the cluster
#'
#' The post-processing rule is the following: the cluster is removed if the mean difference is unsignificant whereas the cluster is replaced by a change-point in the middle if the mean difference is significant. In both cases, some data will be to removed in the next step of the analysis
#
#' @param Tmu the segmentation results obtained from the Segmentation function
#' @param MaxDist the maximal number of days between change-points used to determinate the cluster. Default is 80
#'
#' @return list of two variables (RemoveData and UpdatedCP) :
#' \itemize{
#' \item \code{UpdatedCP} that is the change-points remaining after screening
#' \item \code{RemoveData} that is a data frame including the beginning and the end positions (time index) of the segments to be deleted after filtering
#' }
#'
#' @export


Cluster_screening <- function(Tmu, MaxDist = 80, detail = NULL) {

  RemoveData <-  c()
  UpdatedCP <-  c()
  ChangeCP <- c()


  flag <- integer(length(Tmu$np))
  flag[Tmu$np<MaxDist] <- 1

  ClusterBegInd = which(diff(flag) == 1)+1
  NormalSegInd = which(flag == 0)

  if (length(ClusterBegInd) > 0){
    SegBefInd = sapply(ClusterBegInd, function(x) {
      if (any(NormalSegInd < x)) max(NormalSegInd[NormalSegInd < x]) else NA
    })
    SegAftInd = sapply(ClusterBegInd, function(x) {
      if (any(NormalSegInd > x)) min(NormalSegInd[NormalSegInd > x]) else NA
    })

    ClusterEndInd <-  SegAftInd - 1
    ClusterEndInd[is.na(ClusterEndInd)] <- length(flag)

    RemoveData <-  data.frame(begin = Tmu$begin[ClusterBegInd],end = Tmu$end[ClusterEndInd])
    SegmentsTest = stats::na.omit(data.frame(begin = SegBefInd,end = SegAftInd))
    SegmentsTestOut = data.frame(begin = Tmu$begin[ClusterBegInd],
                                 end = Tmu$end[ClusterEndInd])

    UpdatedCP = Tmu$end[-unique(stats::na.omit(c(which(flag!=0), SegBefInd)))]

    # Test difference in mean before and after cluster if needed ----
    if(nrow(SegmentsTest) > 0) {
      TValues <- sapply(1:nrow(SegmentsTest), function(x) {
        Den = Tmu$mean[SegmentsTest$begin[x]] - Tmu$mean[SegmentsTest$end[x]]
        Nor = sqrt(1 / Tmu$se[SegmentsTest$begin[x]]^2 +
                     1 / Tmu$se[SegmentsTest$end[x]]^2)
        Den / Nor
      })
      PValues <- sapply(TValues, function(x) {
        (stats::pnorm(-abs(x), mean = 0, sd = 1, lower.tail = TRUE)) * 2
      })
      SegmentsTestOut %>%
        mutate(mu_L = Tmu$mean[SegmentsTest$begin],
               mu_R = Tmu$mean[SegmentsTest$end],
               se_L = Tmu$se[SegmentsTest$begin],
               se_R = Tmu$se[SegmentsTest$end,],
               np_L = Tmu$np[SegmentsTest$begin],
               np_R = Tmu$np[SegmentsTest$end],
               tstat = TValues,
               pval = PValues,
               signif = ifelse(PValues > 0.05, 0, 1))
      # Update the segmentation result -----------------------------------------
      ReplacedCP = sapply(1:nrow(SegmentsTest), function(i) {
        if (PValues[i] < 0.05) {
          ceiling((Tmu$end[SegmentsTest$begin[i]] +
                     Tmu$begin[SegmentsTest$end[i]]) / 2)
        }
      })
      # Update of list of changepoints ------------------------------------------
      UpdatedCP = sort(c(UpdatedCP, unlist(ReplacedCP)), decreasing = FALSE)
    }
    ChangeCP <- "Yes"
  } else {
    RemoveData = data.frame(begin = NA,end = NA)
    UpdatedCP = Tmu$end[-nrow(Tmu)]
    ChangeCP <- "No"
  }

  Out = list(UpdatedCP = UpdatedCP,
             RemoveData = RemoveData,
             ChangeCP = ChangeCP)


  if(!is.null(detail)){

  }

  return(Out)

}

