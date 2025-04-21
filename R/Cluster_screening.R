#' Post-processing of the segmentation results aiming to remove clusters of change-points (CPs)
#'
#' Method:
#' (1) detection of cluster of CPs, i.e. consecutive CPs closer than MaxDist days
#' (2) test of the change in mean before and after the cluster with significance level alpha
#' (3) removal of all the CPs in the cluster if the change in mean is unsignificant 
#' (4) add a new change-point in the middle of the cluster if the change in mean is significant. 
#' 
#' Note: it is recommended to replace the data inside the cluster(s) by NA in the general homogenization process
#
#' @param Tmu the segmentation results obtained from the Segmentation function
#' @param MaxDist the maximal number of days between change-points within a cluster. Default is 80.
#' @param detail: if TRUE the output contains a $detail field with additional information on the test. Default is FALSE.
#' @param alpha: significance level of the test (value between 0 and 1). Default is 0.05.
#'
#' @return 
#' \itemize{
#' \item \code{UpdatedCP}: The change-points remaining after screening.
#' \item \code{RemoveData}: A data frame containing the beginning and end positions (time index) of the segments to be deleted after filtering.
#' \item \code{ChangeCP}: "Yes" or "No", indicating whether the list of change-points changed after cluster screening.
#' \item \code{detail}: A data frame containing the results of the significance test of the mean difference before and after the cluster, including the following columns:
#'   \itemize{
#'     \item \code{mu_L}, \code{mu_R}: Means of the segments before and after the cluster.
#'     \item \code{se_L}, \code{se_R}: Standard errors of the means of the segments before and after the cluster.
#'     \item \code{np_L}, \code{np_R}: Number of points before and after the cluster.
#'     \item \code{tstat}, \code{pval}, \code{signif}: t-values, p-values, and significance codes of the test.
#'   }
#' }
#' @export


Cluster_screening <- function(Tmu, alpha = 0.05, MaxDist = 80, detail = FALSE) {

  RemoveData <-  c()
  UpdatedCP <-  c()
  ChangeCP <- c()
  SegmentsTestOut <- NA

  if(nrow(Tmu) > 1) {
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

      RemoveData <- data.frame(begin = Tmu$begin[ClusterBegInd],end = Tmu$end[ClusterEndInd])
      # add the first cluster ----------------------------------------------
      if(flag[1] == 1){
        FirstClusterEnd <- which(flag == 0)[1]-1
        FirstCluster <- data.frame(begin = Tmu$begin[1:FirstClusterEnd],end = Tmu$end[1:FirstClusterEnd])
        RemoveData <- FirstCluster %>%
          rbind(RemoveData)
      }

      SegmentsTest = stats::na.omit(data.frame(begin = SegBefInd,end = SegAftInd))
      SegmentsTestOut = data.frame(begin = Tmu$begin[ClusterBegInd],
                                   end = Tmu$end[ClusterEndInd])
      ind_removed = which(is.na(SegBefInd) | is.na(SegAftInd))
      if(length(ind_removed) > 0) {
        SegmentsTestOut = SegmentsTestOut[-ind_removed,]
      }
      UpdatedCP = Tmu$end[-unique(stats::na.omit(c(which(flag!=0), SegBefInd)))]
      # Test difference in mean before and after cluster if needed ----
      if(nrow(SegmentsTest) > 0) {
        TValues <- sapply(1:nrow(SegmentsTest), function(x) {
          Den = Tmu$mean[SegmentsTest$begin[x]] - Tmu$mean[SegmentsTest$end[x]]
          Nor = sqrt(Tmu$se[SegmentsTest$begin[x]]^2 +
                       Tmu$se[SegmentsTest$end[x]]^2)
          Den / Nor
        })
        PValues <- sapply(TValues, function(x) {
          (stats::pnorm(-abs(x), mean = 0, sd = 1, lower.tail = TRUE)) * 2
        })
        SegmentsTestOut <- SegmentsTestOut %>%
          mutate(mu_L = Tmu$mean[SegmentsTest$begin],
                 mu_R = Tmu$mean[SegmentsTest$end],
                 se_L = Tmu$se[SegmentsTest$begin],
                 se_R = Tmu$se[SegmentsTest$end],
                 np_L = Tmu$np[SegmentsTest$begin],
                 np_R = Tmu$np[SegmentsTest$end],
                 tstat = TValues,
                 pval = PValues,
                 signif = ifelse(PValues > alpha, 0, 1))
        # Update the segmentation result -----------------------------------------
        ReplacedCP = sapply(1:nrow(SegmentsTest), function(i) {
          if (PValues[i] < alpha) {
            ceiling((Tmu$end[SegmentsTest$begin[i]] +
                       Tmu$begin[SegmentsTest$end[i]]) / 2)
          }
        })
        # Update of list of changepoints ------------------------------------------
        UpdatedCP = sort(c(UpdatedCP, unlist(ReplacedCP)), decreasing = FALSE)
        # remove the last point ----------------------------------------------
        if(UpdatedCP[length(UpdatedCP)] == Tmu$end[nrow(Tmu)]) {
          UpdatedCP <- UpdatedCP[-length(UpdatedCP)]
        }
      }

      if(nrow(SegmentsTestOut) == 0){
        SegmentsTestOut <- NA
      }
      ChangeCP <- "Yes"
    } else {
      UpdatedCP = Tmu$end[-nrow(Tmu)]
      RemoveData = data.frame(begin = NA,end = NA)
      ChangeCP <- "No"
    }

  } else {
    RemoveData = data.frame(begin = NA,end = NA)
    ChangeCP <- "No"
  }

  Out = list(UpdatedCP = UpdatedCP,
             RemoveData = RemoveData,
             ChangeCP = ChangeCP)

  if(detail == TRUE){
    Out$detail = SegmentsTestOut
  }

  return(Out)

}

