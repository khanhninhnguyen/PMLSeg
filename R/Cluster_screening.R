#' Post-processing of the segmentation results aiming to remove clusters of change-points (CPs)
#'
#' Method:
#' (1) detection of cluster of CPs, i.e. consecutive CPs closer than MaxDist days
#' (2) test of the change in mean before and after the cluster with significance level alpha
#' (3) removal of all the CPs in the cluster if the change in mean is unsignificant
#' (4) add a new change-point at the beginning of the segment if the change in mean is significant.
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
  ClusterTestOut <- c()

  if(nrow(Tmu) > 1) {
    flag <- integer(length(Tmu$np))
    flag[Tmu$np<MaxDist] <- 1

    # index of first segment in each cluster
    ClusterBegInd = which(diff(c(0, flag)) == 1)

    if (length(ClusterBegInd) > 0){

      NormalSegInd = which(flag == 0)

      # index of segment before and after each cluster (NA if first or last segment)
      SegBefInd = sapply(ClusterBegInd, function(x) {
        if (any(NormalSegInd < x)) max(NormalSegInd[NormalSegInd < x]) else NA
      })
      SegAftInd = sapply(ClusterBegInd, function(x) {
        if (any(NormalSegInd > x)) min(NormalSegInd[NormalSegInd > x]) else NA
      })

      # index of last segment in each cluster
      ClusterEndInd <- SegAftInd - 1
      ClusterEndInd[is.na(ClusterEndInd)] <- length(flag)

      # range of data points which are in each cluster that shall be removed
      RemoveData <- data.frame(begin = Tmu$begin[ClusterBegInd],end = Tmu$end[ClusterEndInd])

      # list of segments before and after that can be used for the test
      SegmentsTest = data.frame(begin = SegBefInd, end = SegAftInd)

      UpdatedCP = Tmu$end[-unique(stats::na.omit(c(which(flag!=0), SegBefInd)))]

      ### Test difference in mean before and after each cluster
      if(nrow(SegmentsTest) > 0) {
        TValues <- sapply(1:nrow(SegmentsTest), function(x) {
          if (is.na(SegmentsTest$begin[x]) | is.na(SegmentsTest$end[x])) NA else
          {
            D = Tmu$mean[SegmentsTest$begin[x]] - Tmu$mean[SegmentsTest$end[x]]
            SD = sqrt(Tmu$se[SegmentsTest$begin[x]]^2 +
                       Tmu$se[SegmentsTest$end[x]]^2)
            D / SD
          }
        })
        PValues <- sapply(TValues, function(x) {
          (stats::pnorm(-abs(x), mean = 0, sd = 1, lower.tail = TRUE)) * 2
        })
        ### save test details in ClusterTestOut df
        ClusterTestOut <- data.frame(
            mu_L = ifelse (is.na(SegmentsTest$begin), NA, Tmu$mean[SegmentsTest$begin]),
            mu_R = ifelse(is.na(SegmentsTest$end), NA, Tmu$mean[SegmentsTest$end]),
            se_L = ifelse (is.na(SegmentsTest$begin), NA, Tmu$se[SegmentsTest$begin]),
            se_R = ifelse(is.na(SegmentsTest$end), NA, Tmu$se[SegmentsTest$end]),
            np_L = ifelse (is.na(SegmentsTest$begin), NA, Tmu$np[SegmentsTest$begin]),
            np_R = ifelse(is.na(SegmentsTest$end), NA, Tmu$np[SegmentsTest$end]),
            tstat = TValues,
            pval = PValues,
            signif = ifelse(PValues > alpha, 0, 1)
         )

        ### Update the CPs which have a significant change in mean
        ind_signif = which(PValues < alpha)
        ReplacedCP = Tmu$begin[SegmentsTest$begin[ind_signif]]#ceiling((Tmu$end[SegmentsTest$begin[ind_signif]] + Tmu$begin[SegmentsTest$end[ind_signif]]) / 2)

        # Update of list of CPs
        UpdatedCP = sort(c(UpdatedCP, unlist(ReplacedCP)), decreasing = FALSE)
        if (length(UpdatedCP)>0){
          if(UpdatedCP[length(UpdatedCP)] == Tmu$end[nrow(Tmu)]) {
            UpdatedCP <- UpdatedCP[-length(UpdatedCP)]
          }
        }
      }

      if(nrow(ClusterTestOut) == 0){
        ClusterTestOut <- NA
      }
      ChangeCP <- "Yes"
    } else {
      UpdatedCP = Tmu$end[-nrow(Tmu)]
      RemoveData = data.frame(begin = NA,end = NA)
      ChangeCP <- "No"
      ClusterTestOut <- NA
    }

  } else {
    RemoveData = data.frame(begin = NA,end = NA)
    ChangeCP <- "No"
    ClusterTestOut <- NA
  }

  Out = list(UpdatedCP = UpdatedCP,
             RemoveData = RemoveData,
             ChangeCP = ChangeCP)

  if(detail == TRUE){
    Out$detail = ClusterTestOut
  }

  return(Out)

}

