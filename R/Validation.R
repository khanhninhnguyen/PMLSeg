#' Validation of change-points using metadata
#'
#' @param OneSeries a data frame, with size n x 2, containing the signal with n points and the dates in Date format. The names of the 2 columns are thus signal and date
#' @param Tmu the segmentation results
#' @param MinDist the minimum distance from change-points to known changes to be validated
#' @param Metadata a data frame with two columns: $date that is the known change in same format as date of \code{OneSeries} and the $type that is the type the change
#'
#' @return a data frame with four columns :
#' \itemize{
#' \item \code{CP} that is the change-points expressed in date to be validated
#' \item \code{closestMetadata} that is the closest date of a known change
#' \item \code{Distance} that is the distance between \code{CP} and \code{closestMetadata}
#' \item \code{type} that is the type of the known change
#' \item \code{valid} that takes the value 1 if the change-point is validated, 0 otherwise using a distance of \code{MinDist}
#' }
#'
#' @import dplyr
#'
#' @export

Validation <- function(OneSeries,Tmu, MinDist = 62, Metadata) {

  if (!inherits( Metadata$date, "Date")) {
    stop("date must be in Date formate")
  }
  distance <- c()
  Distance <- c()
  CP <- c()
  MetadataIndex <- c()

  CP <-  Tmu$end[-nrow(Tmu)]
  MetadataIndex <-  which(OneSeries$date %in% Metadata$date)


  positions_df <- expand.grid(CP = CP, MetadataIndex = MetadataIndex) %>%
    dplyr::mutate(
      distance = mapply(function(x, y) sum(!is.na(OneSeries$signal[x:y])),
                        CP,
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
      valid = ifelse(Distance < MinDist, 1, 0))



  return(Out)
}
