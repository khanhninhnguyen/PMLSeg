#' Validation of change-points using metadata
#'
#' @param OneSeries is a time series data frame with 2 columns, $signal and $date, each of size n x 1
#' @param Tmu is a data frame containing segmentation results (see segmentation.R)
#' @param MaxDist is the maximum distance (number of valid days) between a change-point and the nearest metadata to be valid
#' @param Metadata is a data frame with two columns, $date and $type, they represent changes at the station which observed the OneSeries time series
#' @return a data frame with four columns :
#' \itemize{
#' \item \code{CP} are the dates of the change-points contained in \code{Tmu}
#' \item \code{closestMetadata} are the nearest dates found in \code{Metadata}
#' \item \code{Distance} is the distance between \code{CP} and \code{Metadata}
#' \item \code{type} is the corresponding type of change from \code{Metadata}
#' \item \code{valid} is the validation result: 1 if \code{Distance} is smaller than \code{MaxDist}, 0 otherwise
#' }
#'
#' @import dplyr
#'
#' @export
Validation <- function(OneSeries, Tmu, MaxDist = 62, Metadata) {
  cond1=TRUE
  cond2=TRUE

  if (!inherits( Metadata$date, "Date")) {
    cond1=FALSE
    cat("Metadata$date must be a Date object\n")
  }

  if (nrow(Tmu)==1){
    cond2=FALSE
    cat("No change-point to validate.\n")
  }

  if ((cond1==TRUE) & (cond2==TRUE)){

  distance <- c()
  Distance <- c()
  CP <- c()
  MetadataIndex <- c()



  OneSeriesFull <- OneSeries %>%
    tidyr::complete(date = seq(min(date), max(date), by = "day"))

  CP <-  Tmu$end[-nrow(Tmu)]
  CP_m <- which(OneSeriesFull$date %in% OneSeries$date[CP])

  MetadataIndex <- which(OneSeriesFull$date %in% Metadata$date)

  positions_df <- expand.grid(CP = CP, MetadataIndex = MetadataIndex) %>%
    dplyr::mutate(
      distance = mapply(function(x, y) sum(!is.na(OneSeriesFull$signal[x:y])),
                        CP_m,
                        MetadataIndex),
      distance = distance -1
    )

  closest_results <- positions_df %>%
    dplyr::group_by(CP) %>%
    dplyr::summarise(
      closestMetadata = Metadata$date[which.min(distance)],
      Distance = min(distance)
    ) %>%
    dplyr::ungroup()

  Out <- left_join(closest_results,Metadata,by=c("closestMetadata"="date"))
  Out <- Out %>%
   dplyr:: mutate(
      CP = OneSeries$date[CP],
      valid = ifelse(Distance < MaxDist, 1, 0))



  return(Out)
  }
}

