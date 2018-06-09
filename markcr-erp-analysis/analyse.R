library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lme4)


# Beat data.csv into shape
Df <- read_csv("data.csv") %>% 
  na.omit() %>% # zap the NAs
  mutate(pmq = factor(PMQ_groups, levels=c('low','medium','high'))) %>% # rename a variable, convert to factor
  select(-c(X1, PMQ.T.score, PMQ_groups)) %>% # remove some unnecessary cols
  gather(tmp_condition, erp, -c(Participant.ID, pmq)) %>% # wide to long
  mutate(tmp_condition = str_replace(tmp_condition, 'N300_P[mM]', '')) %>% 
  separate(tmp_condition, c('condition', 'site')) %>% # split a var in two 
  rename(participant = Participant.ID)

# Take a look
ggplot(Df,
       mapping = aes(x = pmq, y = erp)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(condition ~ site)


M <- lmer(erp ~ pmq + (1|participant) + (1|site) + (1|condition),
          data = Df)
     
