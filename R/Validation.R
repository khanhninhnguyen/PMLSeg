#' Validation of change-points using metadata
#'
#' @param OneSeries is a time series data frame with 2 columns, $signal and $date, each of size n x 1
#' @param Tmu is a data frame containing segmentation results (see segmentation.R)
#' @param MaxDist is the maximum distance (number of valid days) between a change-point and the nearest metadata to be valid
#' @param Metadata is a data frame with two columns, $date and $type, they represent changes at the station which observed the OneSeries time series
#'
#' @return
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

  ### check metadata exist and is properly defined
  if ((nrow(Metadata) > 0) & inherits( Metadata$date, "Date")) {
    cond1 = TRUE
  } else {
    cond1 = FALSE
  }

  ### check there is at least one CP to validate
  if (nrow(Tmu) > 1){
    cond2 = TRUE
  } else {
    cond2 = FALSE
  }

  ### validate if metadata exist and there is at least one CP
  if ((cond1==TRUE) & (cond2==TRUE))
  {
    distance <- c()
    Distance <- c()
    CP <- c()
    MetadataIndex <- c()

    OneSeriesFull <- OneSeries %>%
      tidyr::complete(date = seq(min(date), max(date), by = "day"))

    CP <-  Tmu$end[-nrow(Tmu)]
    CP_m <- which(OneSeriesFull$date %in% OneSeries$date[CP])

    MetadataIndex <- which(OneSeriesFull$date %in% Metadata$date)

    if (length(MetadataIndex)==0) {
      print("no metadata available in the time period")
      ### set all valid to 0 if no metadata exist
      valid = Tmu %>%
        mutate(CP = OneSeries$date[Tmu$end],
               closestMetadata = NA,
               type = "U",
               Distance = NA,
               valid = 0)
      valid <- valid[-nrow(valid),]
      valid = valid %>%
        select(CP, closestMetadata, type, Distance, valid)
      Out <- valid
    } else {

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
  }}
  else
  {
    if(cond2==TRUE){
      print("no metadata")
      ### set all valid to 0 if no metadata exist
      valid = Tmu %>%
        mutate(CP = OneSeries$date[Tmu$end],
               closestMetadata = NA,
               type = "U",
               Distance = NA,
               valid = 0)
      valid <- valid[-nrow(valid),]
      valid = valid %>%
        select(CP, closestMetadata, type, Distance, valid)
      Out <- valid
    }
    ### return NULL if there is no CP in the data
    else
    {
      print("no CP to validate")
      Out <- NULL
    }
  }
  return(Out)
}

