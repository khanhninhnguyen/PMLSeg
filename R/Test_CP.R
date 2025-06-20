#' Test of each change-point (CP)
#'
#' Method:
#' (1) test of the change in mean before and after the cluster with significance level alpha
#' (2) removal of all the CPs in the cluster if the change in mean is unsignificant
#'
#' @param Tmu the segmentation results obtained from the Segmentation function
#' @param alpha: significance level of the test (value between 0 and 1). Default is 0.05.
#' @param RemoveDataFromScreening the removed data from the Cluster_screening() function. If no screening has been performed before, \code{RemoveDataFromScreening=NULL}.
#' @param detail: if TRUE the output contains a $detail field with additional information on the test. Default is FALSE.
#' @return
#' \itemize{
#' \item \code{UpdatedCP}: The change-points remaining after testing.
#' \item \code{RemoveData}: The removed data obtained from the screening, NA if no screening is performed before (\code{RemoveDataFromScreening=NULL}).
#' \item \code{ChangeCP}: "Yes" or "No", indicating whether the list of change-points changed after the test.
#' \item \code{detail}: A data frame containing the results of the test of the mean difference before and after the cluster, including the following columns:
#'   \itemize{
#'     \item \code{mu_L}, \code{mu_R}: Means of the segments before and after the change-point.
#'     \item \code{se_L}, \code{se_R}: Standard errors of the means of the segments before and after the change-point.
#'     \item \code{np_L}, \code{np_R}: Number of points before and after the change-point.
#'     \item \code{tstat}, \code{pval}, \code{signif}: t-values, p-values, and significance codes of the test.
#'   }
#' }
#' @export


Test_CP <- function(Tmu, alpha = 0.05,RemoveDataFromScreening=NULL, detail = FALSE) {

  RemoveData <-  c()
  UpdatedCP <-  c()
  ChangeCP <- c()
  ClusterTestOut <- c()

  #RemoveData = data.frame(begin = NA,end = NA)
  if (is.null(RemoveDataFromScreening)){
    RemoveData = data.frame(begin = NA,end = NA)
  } else (RemoveData <- RemoveDataFromScreening)


  if(nrow(Tmu) > 1) {
    # index of segment before and after each cluster (NA if first or last segment)
    SegBefInd = 1:(nrow(Tmu)-1)
    SegAftInd = SegBefInd+1


    SegmentsTest = data.frame(begin = SegBefInd, end = SegAftInd)
    UpdatedCP = Tmu$end[nrow(Tmu)]

    ### Test difference in mean before and after each cluster
    if(nrow(SegmentsTest) > 0) {
      TValues <- sapply(1:nrow(SegmentsTest), function(x) {
      D = Tmu$mean[SegmentsTest$begin[x]] - Tmu$mean[SegmentsTest$end[x]]
      SD = sqrt(Tmu$se[SegmentsTest$begin[x]]^2 +
                        Tmu$se[SegmentsTest$end[x]]^2)
      D / SD
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
        ReplacedCP = Tmu$end[SegmentsTest$begin[ind_signif]]

        # Update of list of CPs
        UpdatedCP = sort(c(UpdatedCP, unlist(ReplacedCP)), decreasing = FALSE)
        if (length(UpdatedCP)>0){
          if(UpdatedCP[length(UpdatedCP)] == Tmu$end[nrow(Tmu)]) {
            UpdatedCP <- UpdatedCP[-length(UpdatedCP)]
          }
        }
    }

    if (length(ind_signif)<(nrow(Tmu)-1)){
      ChangeCP <- "Yes"
    } else {ChangeCP <- "No"}

  } else {
    #RemoveData = data.frame(begin = NA,end = NA)
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

