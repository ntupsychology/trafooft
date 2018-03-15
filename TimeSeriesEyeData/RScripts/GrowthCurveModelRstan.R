# Growth Curve Model with rstan
library(eyetrackingR)
library(rstanarm) # stan_lmer
library(polspline) # For BF
library(bayesplot) # https://github.com/stan-dev/bayesplot
library(dplyr)
options(mc.cores = parallel::detectCores())

# Load data
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

# Bayesian Growth curve model
time_model_o3 <- stan_lmer(Elog ~ Contrast*AOI*(ot1 + ot2 + ot3)  
                           + (1 + ot1 + ot2 + ot3 | subj), 
                               prior_intercept = student_t(df = 1, location = 0), # Priors
                               prior = student_t(df = 1, location = 0),
                               prior_covariance = decov(regularization = 2),
                               data = eye,
                               chains = 3,
                               iter = 1000,
                               cores = 4,
                               seed = 17,
                               adapt_delta = .98)

# Test for convergence
rhat <- summary(time_model_o3)[, "Rhat"]
rhat[round(rhat, 1) != 1]

# Save model
save(time_model_o3,
     file="stanout/timecoursemodel.rda",
     compress="xz")

# Data summary
load(file="stanout/timecoursemodel.rda")

samps <- as.data.frame(time_model_o3) # It saves all the samples from the model.
param <- names(samps)[c(2:16)]

out <- data.frame()
for(p in param){
  post <- samps[,p]
  out <- rbind(out, 
               cbind(Median = median(post), 
                     posterior_interval(time_model_o3, par = p, prob = 0.95)))
}

colnames(out) <- c("Median", "[2.5%", "97.5%]")

rownames(out) <- c( "Noun Contrast", "AOI",  "OT1", "OT2", "OT3", 
                    "Noun Contrast x AOI", "Noun Contrast x OT1",
                    "Noun Contrast x OT2",  "Noun Contrast x OT3",
                    "AOI x OT1", "AOI x OT2", "AOI x OT3",
                    "Noun Contrast x AOI x OT1", "Noun Contrast x AOI x OT2",
                    "Noun Contrast x AOI x OT3"
)
round(out, 2)

param2 <- rownames(out)
names(samps)[c(2:16)] <- param2
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% CrI")
color_scheme_set("red")
mcmc_intervals(samps,
               pars = param2,
               prob_outer = 1,
               prob = .95,
               point_est = "mean"
)

