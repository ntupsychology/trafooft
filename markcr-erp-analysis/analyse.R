library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lme4)
library(lmerTest)

# Beat data.csv into shape
Df <- read_csv("data.csv") %>% 
  na.omit() %>% # zap the NAs
  mutate(pmq_group = factor(PMQ_groups, levels=c('low','medium','high'))) %>% # rename a variable, convert to factor
  rename(pmq_score = PMQ.T.score) %>% 
  select(-c(X1, PMQ_groups)) %>% # remove some unnecessary cols
  gather(tmp_condition, erp, -c(Participant.ID, pmq_score, pmq_group)) %>% # wide to long
  mutate(tmp_condition = str_replace(tmp_condition, 'N300_P[mM]', '')) %>% 
  separate(tmp_condition, c('condition', 'site')) %>% # split a var in two 
  rename(participant = Participant.ID)

# Take a look (by pmq category)
ggplot(Df,
       mapping = aes(x = pmq_group, y = erp)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(condition ~ site)

# Take a look (by pmq score)
ggplot(Df,
       mapping = aes(x = pmq_score, y = erp)) +
  geom_point() +
  facet_grid(condition ~ site)

# Take a look (by pmq score), collapsing over site
ggplot(Df) +
  geom_point(mapping = aes(x = pmq_score, y = erp, col=site)) +
  stat_smooth(method='lm', mapping = aes(x = pmq_score, y = erp)) +
  facet_grid(~condition)


# Use pmq category as predictor
M <- lmer(erp ~ pmq_group + (1|participant) + (1|site) + (1|condition),
          data = Df)

# Use pmq score as predictor
M <- lmer(erp ~ pmq_score + (1|participant) + (1|site) + (1|condition),
          data = Df)

# Test it 
lmerTest::step(M)

