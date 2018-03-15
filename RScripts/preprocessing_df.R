library(ggplot2)
library(eyetrackingR)
library(Matrix)
library(plyr)
library(tidyr)
library(dplyr)

# -----------------------------------------------------------------------------------------
# Load data frame preprocessed for eyetracking-R with both modalities and scaled time axis
# -----------------------------------------------------------------------------------------
load("dfs/sample_report_AOIs.rda") # called "eye"

# Remove invalid responses
eye <- eye %>% filter(Acc == 1
              , eqTP == TRUE
              , Prime == "Target"
              , Modality == "speech"
              , Modifier == "PP"
              , WINDOW == "PRIOR_PRODUCTION_ONSET"
              
              ) %>%
  select(-x,-y, -WINDOW, -Acc, -cond, -eqTP, -textbox, -Scope, -P2, -P3, -P4, -P5, -Modifier, -Modality, -P6a, -P6b, -Prime)
eye$TrackLoss = as.logical(eye$TrackLoss)

colnames(eye) <- c("trial_id", "subj", "time", "scaledTime", "P2", "P1", "TrackLoss", "Contrast")
eye$timeNormal <- eye$time
eye$time <- NULL
AOIs <- c(paste("P",c("1", "2") , sep = ""))
sampleReport <- make_eyetrackingr_data(eye,
                                  participant_column = "subj",
                                  trial_column = "trial_id",
                                  time_column = "scaledTime",
                                  trackloss_column = "TrackLoss",
                                  aoi_columns = AOIs,
                                  treat_non_aoi_looks_as_missing = TRUE
)
start = 0
end = 1

# remove trackloss-ridden trials
sampleReportClean <- clean_by_trackloss(sampleReport,
                                    participant_prop_thresh = .5, trial_prop_thresh = .5,
                                    window_start_time = start, window_end_time = end)

# zoom in on response window
sampleReportWindow <- subset_by_window(sampleReportClean, rezero = FALSE,
                                 window_start_time = start, window_end_time = end)
saveRDS(sampleReportWindow, "dfs/sampleReportWindow.rds")

TimeData <- make_time_sequence_data(sampleReportWindow, time_bin_size = .1, 
                                    predictor_columns = "Contrast",
                                    aois = AOIs,
                                    summarize_by = "subj"
)

saveRDS(sampleReportWindow, "dfs/sampleReportNew.rds")
saveRDS(TimeData, "dfs/TimeDataBySubj.rds")

