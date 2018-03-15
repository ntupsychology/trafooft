# Growth Curve Model
library(lme4)
library(eyetrackingR)

# Load data
# Read time data frame
sampleReportWindow <- readRDS("dfs/sampleReportWindow.rds")
eye <- make_time_sequence_data(sampleReportWindow, time_bin_size = .1, 
                               predictor_columns = "Contrast",
                               aois = AOIs,
                               summarize_by = "subj"
) %>% select(subj, Contrast, Time, AOI, TimeBin, Prop, Elog, ot1:ot7)
head(eye)

# Make variables factors
eye$Contrast <- as.factor(eye$Contrast)
eye$AOI <- as.factor(eye$AOI)

# Data visualisation
plot(eye, predictor_column = "Contrast", dv = "Elog") +
  theme_light()

# Sum contrasts
contrasts(eye$Contrast) <- c(-1, 1)
colnames(contrasts(eye$Contrast)) <- "Noun Contrast"

contrasts(eye$AOI) <- c(-1, 1)
colnames(contrasts(eye$AOI)) <- "AOI"

# Growth curve model
time_model_ot1 <- lmer(Elog ~ Contrast*AOI*ot1 + (1 + ot1 | subj), 
                            data = eye, REML = FALSE)

plot(eye, predictor_column = "Contrast", dv = "Elog", model = time_model_ot1) +
  theme_light()


# Growth curve model
time_model_o2 <- lmer(Elog ~ Contrast*AOI*(ot1 + ot2) + (1 + ot1 + ot2| subj), 
                   data = eye, REML = FALSE)

plot(eye, predictor_column = "Contrast", dv = "Elog", model = time_model_o2) +
  theme_light()

# Growth curve model
time_model_o3 <- lmer(Elog ~ Contrast*AOI*(ot1 + ot2 + ot3) + (1 + ot1 + ot2 + ot3| subj), 
                   data = eye, REML = FALSE)

plot(eye, predictor_column = "Contrast", dv = "Elog", model = time_model_o3) +
  theme_light()

# Data summary
# cleanly show important parts of model (see `summary()` for more)
round(summary(time_model_o3)$coef, 2)
#drop1(time_model_o3, ~., test="Chi")

