#' Test the significance of each change-point (CP)
#'
#' Method:
#' (1) test of the change in mean before and after each change-point (CP) with significance level alpha
#'
#' @param Tmu is a data frame containing the segmentation result (see the Segmentation function).
#' @param detail is a boolean. If TRUE the output contains a $detail field with additional information on the test. Default is FALSE.
#' @param alpha is the significance level of the test (value between 0 and 1). Default is 0.05.
#'
#' @return
#' \itemize{
#' \item \code{UpdatedCP} is a list containing the position of CPs remaining after testing.
#' \item \code{ChangeCP} is a string, "Yes" or "No", indicating whether the test changed the list of CPs.
#' \item \code{detail} is a data frame containing the details of the test including the following columns:
#'   \itemize{
#'     \item \code{mu_L}, \code{mu_R}: Means of the segments to the left and right of the CP.
#'     \item \code{se_L}, \code{se_R}: Standard errors of the means of the segments to the left and right of the CP.
#'     \item \code{np_L}, \code{np_R}: Number of points of the segments to the left and right of the CP.
#'     \item \code{tstat}, \code{pval}, \code{signif}: t-value, p-value, and significance (0 or 1) of the test.
#'   }
#' }
#' @export


Test_CP <- function(Tmu, alpha = 0.05, detail = FALSE) {

    updated_CP <- c()
    changed_CP <- "No"
    test_detail <- NULL

    if(nrow(Tmu) <= 1) {
    warning(" => Test_CP: Nothing to test!")
    # return(NULL)
    } else {
    ### List of indices of segments to the left and right of each CP.
    ind_left = 1:(nrow(Tmu)-1)
    ind_right = ind_left+1
    list_ind = data.frame(left = ind_left, right = ind_right)

    ### Test difference in mean to the left and right of each CP and update the list of CPs.
    CP = Tmu$end[1:(nrow(Tmu)-1)]
    TValues <- sapply(1:nrow(list_ind), function(x) {
      D = Tmu$mean[list_ind$left[x]] - Tmu$mean[list_ind$right[x]]
      SD = sqrt(Tmu$se[list_ind$left[x]]^2 + Tmu$se[list_ind$right[x]]^2)
      D / SD
    })
    PValues <- sapply(TValues, function(x) {
      (stats::pnorm(-abs(x), mean = 0, sd = 1, lower.tail = TRUE)) * 2
    })

    ### save test details
    test_detail <- data.frame(
      mu_L = ifelse(is.na(list_ind$left), NA, Tmu$mean[list_ind$left]),
      mu_R = ifelse(is.na(list_ind$right), NA, Tmu$mean[list_ind$right]),
      se_L = ifelse(is.na(list_ind$left), NA, Tmu$se[list_ind$left]),
      se_R = ifelse(is.na(list_ind$right), NA, Tmu$se[list_ind$right]),
      np_L = ifelse(is.na(list_ind$left), NA, Tmu$np[list_ind$left]),
      np_R = ifelse(is.na(list_ind$right), NA, Tmu$np[list_ind$right]),
      tstat = TValues,
      pval = PValues,
      signif = ifelse(PValues > alpha, 0, 1)
    )

    ### replace the CPs whith those having a significant change in mean
    ind_signif = which(PValues < alpha)
    updated_CP = CP[list_ind$left[ind_signif]]

    ### set changed_CP
    if(length(updated_CP) < length(CP)) changed_CP <- "Yes"
    }

    ### Create outout list
    Out = list(UpdatedCP = updated_CP,
             ChangeCP = changed_CP,
             detail = test_detail)

    return(Out)
}
