library(eyetrackingR)
library(dplyr)

# Read time data frame
sampleReportWindow <- readRDS("dfs/sampleReportWindow.rds")
eye <- make_time_sequence_data(sampleReportWindow, time_bin_size = .1, 
                                    predictor_columns = "Contrast",
                                    aois = AOIs,
                                    summarize_by = "subj"
) %>% select(subj, Contrast, AOI, TimeBin, Prop, SamplesInAOI, SamplesTotal, ot1:ot7)
head(eye)

#----
# Empirical logits: Quasi linear regression
# Unbound and relative to the overall number of events
# elog = log( (Prop + e) / (Total - Prop + e) ) 
# e = a priori probability (.5 for 2 AOIs)  
eye$Elog <- log( 
  (eye$SamplesInAOI + .5) / 
    (eye$SamplesTotal - eye$SamplesInAOI + .5) 
  )

summary(eye$Prop)
summary(eye$Elog)

#----
# Orthogonal polynomial timecodes. 
# The linear, quadratic, cubic, etc. component of our Time predictor (so Time, Time^2, Time^3, etc.). 
# These transformations are uncorrelated with each other, and therefore more appropriate for multiple regression.

# e.g.
# lm(y ~ group*(time + time^2 + time^3 ...), data)

# Correlation of time values raised to the power of...
p <- data.frame(time = 1:10)
p$time2 <- p$time^2
p$time3 <- p$time^3
p$time4 <- p$time^4
p$time5 <- p$time^5
p$time6 <- p$time^6
p$time7 <- p$time^7

round(cor(p),2)

ggplot(p, aes(x=time, y=time2)) +
  geom_line() +
  geom_line(aes(y=time3), color='red') +    # time^3
  geom_line(aes(y=time4), color='blue') +   # time^4
  geom_line(aes(y=time5), color='green') +  # time^5
  geom_line(aes(y=time6), color='purple') + # time^6
  geom_line(aes(y=time7), color='yellow') + # time^7
  scale_x_continuous(name="") +
  scale_y_continuous(name="")

# Orthogonal polynomial timecodes
p2 <- data.frame(time = 1:10)
p2 <- cbind(p2, poly(p2$time, 7))
round(cor(p2[,c(2:8)]),2)

colnames(p2) <- c("time", paste("ot",1:7, sep=""))
ggplot(p2, aes(x=time, y=ot1)) +
  geom_line() +
  geom_line(aes(y=ot2), color='red') +    # quadratic
  geom_line(aes(y=ot3), color='blue') +   # cubic
  geom_line(aes(y=ot4), color='green') +  # quartic
  geom_line(aes(y=ot5), color='purple') + # quintic 
  geom_line(aes(y=ot6), color='yellow') + # sextic
  geom_line(aes(y=ot7), color='pink') +   # septic
  scale_x_continuous(name="") +
  scale_y_continuous(name="")


# generate dataframe summarizing values of each vector
timecodes <- unique(eye[, c('ot1','ot2','ot3','ot4','ot5','ot6','ot7')])
timecodes$num <- 1:nrow(timecodes)
round(cor(timecodes[, c(1:7)]),2)
ggplot(timecodes, aes(x=num, y=ot1)) +
  geom_line() +
  geom_line(aes(y=ot2), color='red') +    # quadratic
  geom_line(aes(y=ot3), color='blue') +   # cubic
  geom_line(aes(y=ot4), color='green') +  # quartic
  geom_line(aes(y=ot5), color='purple') + # quintic 
  geom_line(aes(y=ot6), color='yellow') + # sextic
  geom_line(aes(y=ot7), color='pink') +   # septic
  scale_x_continuous(name="") +
  scale_y_continuous(name="")


