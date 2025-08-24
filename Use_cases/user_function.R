###########################################################################################
### this script runs the PMLseg functions for use cases
###########################################################################################

library(purrr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(PMLseg)

### create output paths if they don't exist
if (!dir.exists(path_result)) {
  dir.create(path_result)
}
if (!dir.exists(path_plots)) {
  dir.create(path_plots)
}

### load metadata file
print(sprintf("Load metadata file %s...", filename_metadata))
buffer = read.table(file = filename_metadata, header = TRUE) %>% 
  setNames(c("name", "year", "doy", "date", "type")) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))
Metadata = buffer %>% select(all_of(c("name", "date", "type")))

### select metadata for the station
station_metadata = Metadata %>% filter(name ==  station_name)
print(sprintf(" > found %d metadata events for station %s", nrow(station_metadata), station_name))
print(station_metadata)

### load data for the station
path_data_one <- paste0(path_data, station_name, ".txt")
print(sprintf("Load data file %s...", path_data_one))
buffer <- read.table(
    file = path_data_one,
    sep = "\t",
    header = TRUE,
    na.strings = NaN,
    colClasses = c("character", "numeric"),
    col.names = c("date", "signal")
  )
buffer$date <- as.Date(buffer$date, format = "%Y-%m-%d")
OneSeries = buffer %>% select(all_of(c("date", "signal")))

### data info
n <- length(OneSeries$date)
np <- sum(!is.na(OneSeries$signal))
date_begin <- OneSeries$date[1]
date_end <- OneSeries$date[n]
days_diff <- as.numeric(difftime(date_end, date_begin, units = "days"))
completeness <- np / (days_diff + 1) * 100
print(sprintf(" > data summary: n=%d, np=%d, dates=%s..%s (%d days), completeness=%.2f%%", n, np, date_begin, date_end, days_diff, completeness))

### Run segmentation
print(sprintf("Run segmentation for station %s with selectionK = %s, selectionF = %s, FunctPart = %s, VarMonthly = %s, Kmax = %d ...", station_name, selectionK, selectionF, FunctPart, VarMonthly, Kmax))
seg = Segmentation(OneSeries = OneSeries, selectionK = selectionK, selectionF = selectionF, FunctPart = FunctPart, VarMonthly = VarMonthly, Kmax = Kmax)

### Loop on criteria (if selectionK == "All")
if(selectionK == "All") { 
    liste_criterion = names(seg$Tmu) 
} else { 
    liste_criterion = selectionK 
}
seg_tmp <- c()
for (criterion in liste_criterion) {
    if(selectionK == "All") {
        seg_tmp$Tmu = seg$Tmu[[criterion]]
        seg_tmp$CoeffF = seg$CoeffF[[criterion]]
        seg_tmp$SSR = seg$SSR[[criterion]]
        seg_tmp$FitF = seg$FitF[[criterion]]
        seg_tmp$MonthVar = seg$MonthVar
    } else {
        seg_tmp <- seg
    }
    pp <- nrow(seg_tmp$Tmu)+length(seg_tmp$CoeffF)
    print(sprintf(" > segmentation results: criterion = %s, K = %d, min(mu) = %.2f, max(mu) = %.2f, rms(MonthVar) = %.2f, rss(CoeffF) = %.2f, sqrt(SSR/dof) = %.2f", criterion, length(seg_tmp$Tmu$begin), min(seg_tmp$Tmu$mean), max(seg_tmp$Tmu$mean), sqrt(mean(seg_tmp$MonthVar)), sqrt(sum(seg_tmp$CoeffF^2)), sqrt(seg_tmp$SSR/(np-pp))))

    # print(" > segmentation details:")
    # print(seg_tmp$Tmu)
    # print(sqrt(seg_tmp$MonthVar))
    # print(sqrt(mean(seg_tmp$MonthVar)))
    # print(seg_tmp$SSR)

    ### run validation
    valid = Validation(OneSeries = OneSeries, Tmu = seg_tmp$Tmu, Metadata = station_metadata, MaxDist = max_dist_validation)
    print(sprintf(" > validation results: station = %s, criterion = %s, detected change-points = %d, metadata = %d, validated = %d", station_name, criterion, nrow(seg_tmp$Tmu)-1, nrow(station_metadata), sum(valid$valid)))

    ### plot with validation results
    mytitle <- sprintf("Station %s, %s, before screening", station_name, criterion)
    p1 <- PlotSeg(OneSeries = OneSeries, SegRes = seg_tmp, FunctPart = TRUE, labelx = "", labely = mylabely, Metadata = station_metadata, Validated_CP_Meta = valid, title = mytitle)
    print(p1)

    ### cluster screening
    screening <- Cluster_screening(Tmu = seg_tmp$Tmu, MaxDist = max_dist_cluster, alpha = alpha_cluster, detail = TRUE)

    ### if some CPs have been removed
    if (screening$ChangeCP == "Yes"){
        print(sprintf(" > screening results: station = %s, criterion = %s => removed %d segment(s)", station_name, criterion, nrow(screening$RemoveData)))
        print(screening)
        
        ### add tbegin and tend to RemoveData
        screening$RemoveData = screening$RemoveData %>% mutate(tbegin = OneSeries$date[screening$RemoveData$begin], tend = OneSeries$date[screening$RemoveData$end])

        ### update the time series
        OneSeries_updated <- UpdateTimeSeries(OneSeries, screening$RemoveData)

        ### update the segmentation parameters after screening
        seg_updated <- UpdatedParametersForFixedCP(OneSeriesUpd = OneSeries_updated, UpdatedCP = screening$UpdatedCP, FunctPart = TRUE)
        
        ### validate again
        valid_updated = Validation(OneSeries = OneSeries_updated, Tmu = seg_updated$Tmu, Metadata = station_metadata, MaxDist = max_dist_validation)
        print(sprintf(" > validation results (after screening): station = %s, criterion = %s, detected change-points = %d, metadata = %d, validated = %d", station_name, criterion, nrow(seg_updated$Tmu)-1, nrow(station_metadata), sum(valid_updated$valid)))
        
    } else {
        print(sprintf(" > screening results: station = %s, criterion = %s => nothing removed", station_name, criterion))
        OneSeries_updated <- OneSeries
        seg_updated <- seg_tmp
        valid_updated <- valid
    }

    ### plot with validation results
    mytitle <- sprintf("Station %s, %s, after screening", station_name, criterion)
    p <- PlotSeg(OneSeries = OneSeries_updated, SegRes = seg_updated, FunctPart = TRUE, labelx = NULL, labely = mylabely, Metadata = station_metadata, Validated_CP_Meta = valid_updated, title = mytitle)
    print(p)

    ### save plot
    file_name = file.path(path_plots, paste0(station_name, ".", criterion, ".png"))
    ggplot2::ggsave(file_name, plot = p, width = 8, height = 4, units = "in")
}