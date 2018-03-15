library(eyetrackingR)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
head(eye <- readRDS("dfs/sampleReportNew.rds"))
AOIs <- c("P1", "P2")

subj1 <- eye %>% 
  dplyr::filter(trial_id == "12_1_16_Target") %>%
  gather(key = AOIs, value = Looks, na.rm = TRUE, P1, P2)

#----
# Raw samples
p <- ggplot(data= subset(subj1, Looks == TRUE), 
            aes(x = timeNormal, y = AOIs, group = 1)) + 
  geom_line() + 
  theme_bw(base_size = 13) +
  xlab("Time"); p


#----
# Aggregate across planning window
head(AggrData <- make_time_window_data(eye, 
                             aois = AOIs, 
                             predictor_columns=c('Contrast'),
                             summarize_by = "trial_id")) %>% select(-Weights, -LogitAdjusted, -ArcSin, -Elog)

p <- ggplot(data = AggrData, 
            aes(x = Contrast, y = Prop)) 
p <- p + facet_grid(. ~ AOI) + geom_boxplot() + theme_bw(base_size = 13)
p <- p + ylab("Proportion of looks to AOIs"); p


#----
# Aggregate time bins
sampleReportWindow <- readRDS("dfs/sampleReportWindow.rds")
TimeData <- make_time_sequence_data(sampleReportWindow, time_bin_size = .1, 
                                    predictor_columns = "Contrast",
                                    aois = AOIs,
                                    summarize_by = "subj"
) %>% select(subj, Contrast, AOI, TimeBin, Prop)
head(TimeData)

# Summarise
M <- TimeData %>% group_by(Contrast, AOI, TimeBin) %>%
  dplyr::summarise(M = mean(Prop, na.rm = TRUE),
            SE = sd(Prop, na.rm = TRUE)/sqrt(length(Prop)) )

p <- ggplot(data = M, 
            aes(x = TimeBin, y = M, group = AOI, fill = AOI)) 
p <- p + facet_grid(Contrast ~ .) + theme_bw(base_size = 13) + geom_blank()
p <- p + geom_ribbon(aes(max = M + 1.96*SE, min = M - 1.96*SE), alpha = .8, show.legend = TRUE ) 
p <- p + scale_fill_hue(l=20)
p <- p + ylab("Proportion of looks to AOIs"); p

