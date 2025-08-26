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

### Run Segmentation
print(sprintf("Run segmentation for station %s with selectionK = %s, selectionF = %s, FunctPart = %s, VarMonthly = %s, Kmax = %d ...", station_name, selectionK, selectionF, FunctPart, VarMonthly, Kmax))
Seg = Segmentation(OneSeries = OneSeries, selectionK = selectionK, selectionF = selectionF, FunctPart = FunctPart, VarMonthly = VarMonthly, Kmax = Kmax)

### Loop on criteria (if selectionK == "All")
if(selectionK == "All") { 
    liste_criterion = names(Seg$Tmu) 
} else { 
    liste_criterion = selectionK 
}
SegCrit <- c()
for (criterion in liste_criterion) {
    if(selectionK == "All") {
        print(sprintf("Loop over selectionK:"))
        SegCrit$Tmu = Seg$Tmu[[criterion]]
        SegCrit$CoeffF = Seg$CoeffF[[criterion]]
        SegCrit$SSR = Seg$SSR[[criterion]]
        SegCrit$FitF = Seg$FitF[[criterion]]
        SegCrit$MonthVar = Seg$MonthVar
    } else {
        SegCrit <- Seg
    }
    
    ### display Segmentation results
    pp <- nrow(SegCrit$Tmu)+length(SegCrit$CoeffF)
    print(sprintf(" > criterion = %s, station = %s, segmentation detected K = %d segments, min(mu) = %.2f, max(mu) = %.2f, rms(MonthVar) = %.2f, rss(CoeffF) = %.2f, sqrt(SSR/dof) = %.2f", criterion, station_name, length(SegCrit$Tmu$begin), min(SegCrit$Tmu$mean), max(SegCrit$Tmu$mean), sqrt(mean(SegCrit$MonthVar)), sqrt(sum(SegCrit$CoeffF^2)), sqrt(SegCrit$SSR/(np-pp))))

    # print(" > Segmentation details:")
    # print(SegCrit$Tmu)
    # print(sqrt(SegCrit$MonthVar))
    # print(sqrt(mean(SegCrit$MonthVar)))
    # print(SegCrit$SSR)

    ### run validation before screening
    ValidRes = Validation(OneSeries = OneSeries, Tmu = SegCrit$Tmu, Metadata = station_metadata, MaxDist = max_dist_validation)
    print(sprintf(" > criterion = %s, station = %s, validation of change-points: detected = %d, metadata = %d, validated = %d", criterion, station_name, nrow(SegCrit$Tmu)-1, nrow(station_metadata), sum(ValidRes$valid)))

    ### cluster screening
    ScreeningRes <- Cluster_screening(Tmu = SegCrit$Tmu, MaxDist = max_dist_cluster, alpha = alpha_cluster, detail = TRUE)

    ### if some CPs have been removed
    if(ScreeningRes$ChangeCP == "Yes"){
        print(sprintf(" > criterion = %s, station = %s, screening removed %d segment(s)", criterion, station_name, nrow(ScreeningRes$RemoveData)))
        print(ScreeningRes$detail)
        
        ### add tbegin and tend to RemoveData
        # ScreeningRes$RemoveData = ScreeningRes$RemoveData %>% mutate(tbegin = OneSeries$date[ScreeningRes$RemoveData$begin], tend = OneSeries$date[ScreeningRes$RemoveData$end])

        ### update the time series
        OneSeriesUpd <- UpdateTimeSeries(OneSeries, ScreeningRes$RemoveData)

        ### update the segmentation parameters after screening
        SegScrUpd <- UpdatedParametersForFixedCP(OneSeriesUpd = OneSeriesUpd, UpdatedCP = ScreeningRes$UpdatedCP, FunctPart = FunctPart)
                
    } else {
        print(sprintf(" > criterion = %s, station = %s, screening removed nothing ", criterion, station_name))
        OneSeriesUpd <- OneSeries
        SegScrUpd <- SegCrit
    }

    ### validate after screening
    ValidResScr = Validation(OneSeries = OneSeriesUpd, Tmu = SegScrUpd$Tmu, Metadata = station_metadata, MaxDist = max_dist_validation)
    print(sprintf(" > criterion = %s, station = %s, validation of change-points (after screening): detected = %d, metadata = %d, validated = %d", criterion, station_name, nrow(SegScrUpd$Tmu)-1, nrow(station_metadata), sum(ValidResScr$valid)))

    ### test the significance of the change-points 
    TestRes <- Test_CP(Tmu = SegScrUpd$Tmu, alpha = alpha_test_CP, detail = TRUE)

    ### if some CPs are not significant
    if(TestRes$ChangeCP == "Yes"){
        print(sprintf(" > criterion = %s, station = %s, test of change-points: updated number = %d", criterion, station_name, nrow(TestRes$UpdatedCP)))
        print(TestRes)
        
        ### update the segmentation parameters after test
        SegScrTestUpd <- UpdatedParametersForFixedCP(OneSeriesUpd = OneSeriesUpd, UpdatedCP = TestRes$UpdatedCP, FunctPart = FunctPart)
    
    } else {
        print(sprintf(" > criterion = %s, station = %s, test changed nothing", criterion, station_name))
        SegScrTestUpd <- SegScrUpd
    }

    ### validate again
    ValidResScrTest = Validation(OneSeries = OneSeriesUpd, Tmu = SegScrTestUpd$Tmu, Metadata = station_metadata, MaxDist = max_dist_validation)
    print(sprintf(" > criterion = %s, station = %s, validation of change-points (after screening and test): detected = %d, metadata = %d, validated = %d", criterion, station_name, nrow(SegScrTestUpd$Tmu)-1, nrow(station_metadata), sum(ValidResScrTest$valid)))

    ### plot before screening
    mytitle <- sprintf("Station %s, %s, before screening", station_name, criterion)
    p1 <- PlotSeg(OneSeries = OneSeries, SegRes = SegCrit, FunctPart = FunctPart, labelx = "", labely = mylabely, Metadata = station_metadata, Validated_CP_Meta = ValidRes, title = mytitle)
    print(p1)

    ### plot after screening
    mytitle <- sprintf("Station %s, %s, after screening", station_name, criterion)
    p2 <- PlotSeg(OneSeries = OneSeriesUpd, SegRes = SegScrUpd, FunctPart = FunctPart, labelx = NULL, labely = mylabely, Metadata = station_metadata, Validated_CP_Meta = ValidResScr, title = mytitle)
    print(p2)

    ### plot after screening and test
    mytitle <- sprintf("Station %s, %s, after screening and test", station_name, criterion)
    p3 <- PlotSeg(OneSeries = OneSeriesUpd, SegRes = SegScrTestUpd, FunctPart = FunctPart, labelx = NULL, labely = mylabely, Metadata = station_metadata, Validated_CP_Meta = ValidResScrTest, title = mytitle)
    print(p3)

    ### save plots
    file_name = file.path(path_plots, paste0(project, ".", criterion, ".", station_name, ".seg_valid.png"))
    ggplot2::ggsave(file_name, plot = p1, width = 8, height = 4, units = "in")
    file_name = file.path(path_plots, paste0(project, ".", criterion, ".", station_name, ".seg_screened_valid.png"))
    ggplot2::ggsave(file_name, plot = p2, width = 8, height = 4, units = "in")
    file_name = file.path(path_plots, paste0(project, ".", criterion, ".", station_name, ".seg_screened_tested_valid.png"))
    ggplot2::ggsave(file_name, plot = p3, width = 8, height = 4, units = "in")
}
