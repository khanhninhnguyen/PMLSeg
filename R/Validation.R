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
    # select metadata in the period of data or nearest to the left and right
    ind = which(Metadata$date < min(OneSeries$date))
    if(length(ind)>0) {
      Metadata = Metadata[max(ind):length(Metadata),]
    } 
    ind = which(Metadata$date > max(OneSeries$date))
    if(length(ind)>0) {
      Metadata = Metadata[1:min(ind),]
    }
    # K = number of segments, M = number of metadata
    K = nrow(Tmu);
    M = nrow(Metadata);
    CP = Tmu$tend[1:(K-1)]
    Distance <- vector("numeric", K-1)
    Meta <- vector("numeric", K-1)
    
    # squeeze OneSeries (remove dates when signal == NA)
    OneSeriesRed = OneSeries[which(!is.na(OneSeries$signal)),]
    
    # search nearest metadata date for each CP
    d <- matrix(nrow = K-1, ncol = M)
    for(k in 1:K-1) {
      dd = abs(OneSeriesRed$date - CP[k])
      i_k = which.min(dd)
      for(j in 1:M) {
        dd = abs(OneSeriesRed$date - Metadata$date[j])
        i_j = which.min(dd)
        d[k, j] = abs(i_k - i_j)
      }
      j_k = which.min(d[k,])
      Meta[k] = j_k
      Distance[k] = d[k, j_k]
    }
    
    # create output data frame
    Out <- data.frame(CP = CP, 
      closestMetadata = Metadata$date[Meta], 
      type = Metadata$type[Meta], 
      Distance = Distance, 
      valid = ifelse(Distance < MaxDist, 1, 0))
  }
  else
  {
    if(cond2==TRUE){
      print("the station has no metadata")
      ### set all valid to 0 if no metadata exist
      CP = Tmu$tend
      valid = Tmu %>%
        mutate(CP = CP,
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

