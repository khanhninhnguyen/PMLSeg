#' Plot the time series with segmentation results: change-point positions, estimatedmean , with the monthly variance and function (if specified in the model)
#'
#' @param OneSeries a data frame, with size n x 2, containing the signal with n points and the dates in Date format. The names of the 2 columns are thus signal and date
#' @param SegRes result from the segmentation funtion.
#' @param FunctPart a boolean, specifies if the functional part should be plotted. Default is FALSE.
#' @param RemoveData a data frame including the beginning and the end positions (time index) of the segments to be removed.
#' @param Validated_CP_Meta the results from the Validation function. Default is NULL.
#' @param Metadata a data frame with two columns: $date (in date format) and $type (label in string format)
#' @param labelx a string for the label of the x-axis
#' @param labely a string for the label of the y-axis
#'
#' @return a object of class \code{\link[ggplot2]{ggplot}} representing the
#'   time series plot with segmentation and validation results overlaid.
#'
#' @import ggplot2
#' @importFrom dplyr mutate %>% group_by summarise
#'
#' @export
#'
PlotSeg <- function(OneSeries,
                    SegRes,
                    FunctPart = TRUE,
                    RemoveData = NULL,
                    Metadata = NULL,
                    Validated_CP_Meta = NULL,
                    labelx = "Date",
                    labely = "signal") {


  x_period = "1 year"

  MaxPoint = max(OneSeries$signal, na.rm = TRUE) + 0.5
  MinPoint = min(OneSeries$signal, na.rm = TRUE) - max(max(sqrt(SegRes$MonthVar)), max(SegRes$FitF, na.rm = TRUE)) - 0.5

  if (!is.null(RemoveData) && is.na(RemoveData$begin[1])) {
    RemoveData = NULL
  }
  Month <- c()
  type <- c()
  value <- c()
  variable <- c()
  shapes <- c()

  colors <- c("signal" = "gray", "Mean" = "red", "FitF" = "purple", "MonthStd" = "cyan")

  OneSeries <- OneSeries %>%
    mutate(Mean = rep(SegRes$Tmu$mean, times = (SegRes$Tmu$end - SegRes$Tmu$begin + 1)),
           Month = as.numeric(format(date, "%m")),
           MonthStd = sqrt(SegRes$MonthVar[as.numeric(Month)]) - abs(MinPoint)) %>%
    select(-Month)

  if (FunctPart==TRUE){
    OneSeries <- OneSeries %>%
      mutate( FitF = SegRes$FitF - abs(MinPoint))
  }


  # validated CP
  if (!is.null(Validated_CP_Meta)) {
    validCP = Validated_CP_Meta$CP[which(Validated_CP_Meta$valid == 1)]
    OneSeries <- OneSeries %>%
      mutate(valid = ifelse(date %in% validCP, MinPoint, NA))
    shapes <- c(shapes, "valid" = 17)
  }

  # Remove cluster
  if (!is.null(RemoveData)) {
    RemoveInd <- unlist(lapply(1:nrow(RemoveData), function(x) {
      RemoveData$begin[x]:RemoveData$end[x]
    }))

    OneSeries[RemoveInd, c("signal", "Mean")] <- NA
  }

  is_na_signal <- is.na(OneSeries$signal)
  if(sum(is_na_signal) > 0){
    if (FunctPart) {
      cols_to_update <- c("MonthStd", "Mean", "FitF")
    } else {
      cols_to_update <- c("MonthStd", "Mean")
    }

    if(all(cols_to_update %in% names(OneSeries))) {
      OneSeries[is_na_signal, cols_to_update] <- NA
    } else {
      warning("One or more columns specified in cols_to_update do not exist in OneSeries.")
    }
  }

  # Expand the series to be continuous
  OneSeries <- OneSeries %>%
    tidyr::complete(date = seq(min(date), max(date), by = "day"))

  # Add metadata
  if (!is.null(Metadata)) {
    # Metadata$type <- paste0("Meta", Metadata$type)
    types = unique(Metadata$type)
    OneSeries[types] <- NA

    MetaShape = stats::setNames(c(1:length(types)), types)
    shapes <- c(shapes, MetaShape)

    for (type in types) {
      spec_date = Metadata$date[Metadata$type == type] %>% unlist()
      OneSeries[[type]][OneSeries$date %in% spec_date] <- MaxPoint
    }

  } else{
    OneSeries <- OneSeries %>%
      dplyr::mutate( Meta = ifelse(date %in% Metadata, MaxPoint, NA))
    shapes <- c(shapes, "Meta" = 1)
  }
  # make data for plot

  variable_names <- names(OneSeries)[-1]  # Exclude the first column which is 'date'

  long_data <- OneSeries %>%
    tidyr::pivot_longer(
      cols = all_of(variable_names), # Specify the columns to pivot
      names_to = "variable",       # Name of the new column holding the original column names
      values_to = "value"          # Name of the new column holding the values
      # 'date' column is automatically kept as an identifier
    )
  long_data$variable = factor(long_data$variable, levels = variable_names)

  p <- ggplot2::ggplot(long_data, aes(x = date, y = value, colour = variable, shape = variable)) +
    theme_bw() +
    geom_line(data = subset(long_data, variable %in% c("signal")),
              aes(color = variable), size = 0.5, na.rm = TRUE) +
    geom_line(data = subset(long_data, variable %in% c("Mean", "MonthStd")),
              aes(color = variable), size = 0.5, na.rm = TRUE) +
    geom_hline(yintercept = 0, size = 0.3, lty = 1, color = "black",na.rm = TRUE) +
    geom_hline(yintercept = MinPoint, size = 0.3, lty = 1, color = "black",na.rm = TRUE)


  if (FunctPart==TRUE){
    p <- p + geom_line(data=subset(long_data, variable == "FitF"), size = 0.5, na.rm = TRUE)
  }

  # Add changepoints detected by segmentation
  if (nrow(SegRes$Tmu) > 1) {
    p <- p +
      geom_vline(xintercept = OneSeries$date[SegRes$Tmu$end[-nrow(SegRes$Tmu)]],
                 size = 0.5, lty = 2, color = "black", alpha = 0.6,na.rm = TRUE)
  }

  if (!is.null(Validated_CP_Meta)) {
    p <- p +
      geom_point(data = subset(long_data, variable == "valid"), color = "coral", aes(shape = variable),
                 size = 2.5,na.rm = TRUE)
  }

  if (!is.null(Metadata)) {
    if (!is.null(Metadata$type)) {
      p <- p +
        geom_point(data = subset(long_data, variable %in% types), color = "coral", aes(shape = variable),
                   size = 2, na.rm = TRUE)
    } else {
      p <- p +
        geom_point(data = subset(long_data, variable == "Meta"), color = "coral",  aes(shape = variable),
                   size = 2, na.rm = TRUE)
    }
  }

  if (!is.null(Metadata)){
    p <- p +
      scale_x_date(
        limits = as.Date(c(OneSeries$date[1], OneSeries$date[nrow(OneSeries)])),
        breaks = function(x) seq.Date(from = OneSeries$date[1], to = OneSeries$date[nrow(OneSeries)], by = x_period),
        date_labels = "%Y") +
      scale_color_manual(values = colors) +
      scale_shape_manual(values = shapes) +
      labs(x = labelx, y = labely, color = "", fill = "") +
      theme(
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      )
  } else {
    p <- p +
      scale_x_date(
        limits = as.Date(c(OneSeries$date[1], OneSeries$date[nrow(OneSeries)])),
        breaks = function(x) seq.Date(from = OneSeries$date[1], to = OneSeries$date[nrow(OneSeries)], by = x_period),
        date_labels = "%Y") +
      scale_color_manual(values = colors) +
      labs(x = labelx, y = labely, color = "", fill = "") +
      theme(
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      )
  }

  p
  return(p)
}

