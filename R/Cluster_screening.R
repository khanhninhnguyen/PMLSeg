#' Post-processing, called screening, of the detected change-points
#'
#' (1) detection of cluster of change-points (formed by consecutive change-points separated by a maximum number of days)
#' (2) testing the variation of mean before and after the cluster
#'
#' The post-processing rule is the following: the cluster is removed if the mean difference is unsignificant whereas the cluster is replaced by a change-point in the middle if the mean difference is significant. In both cases, some data will be to removed in the next step of the analysis
#
#' @param Tmu the segmentation results obtained from the Segmentation function
#' @param MaxDist the maximal number of days between change-points used to determinate the cluster. Default is 80
#' @param detail any object indicating to have detail of test as output. Defaul is NULL, do not include the details.
#' @param p_val a number from 0 to 1 indicating the threshold for p-value
#'
#' @return A list of four variables (RemoveData, UpdatedCP, ChangeCP, and detail):
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


Cluster_screening <- function(Tmu, p_val = 0.05, MaxDist = 80, detail = NULL) {

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

      RemoveData <-  data.frame(begin = Tmu$begin[ClusterBegInd],end = Tmu$end[ClusterEndInd])
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
                 signif = ifelse(PValues > p_val, 0, 1))
        # Update the segmentation result -----------------------------------------
        ReplacedCP = sapply(1:nrow(SegmentsTest), function(i) {
          if (PValues[i] < p_val) {
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

  if(!is.null(detail)){
    Out$detail = SegmentsTestOut
  }

  return(Out)

}

