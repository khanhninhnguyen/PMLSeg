#' Post-processing of the time series after cluster screening
#'
#' Method:
#' (3) replaces all the data points in the clusters detected by Cluster_screening() by NA
#'
#' @param OneSeries A time series data frame with 2 columns, $signal and $date, each of size n x 1
#' @param RemoveData A data frame containing the beginning and end positions (time index) of the segments to be deleted in the data.
#'
#' @return UpdatedSeries The time series after scrrening
#'
#' @export


UpdateTimeSeries <- function(OneSeries, RemoveData){

  DataScreened <- OneSeries
  
  if (!any(is.na(RemoveData))) {
    for (i in 1:(nrow(RemoveData))) {
        DataScreened$signal[RemoveData$begin[i]:RemoveData$end[i]] = NA
    }
  }

  return(DataScreened)
}