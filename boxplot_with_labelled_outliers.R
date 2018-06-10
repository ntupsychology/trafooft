library(dplyr)
library(ggplot2)
library(magrittr)

set.seed(151)

N <- 45
foo <-  data.frame(ID = 1:N,
                   scores = 50 + rt(N, df=3) * 10)

foo[11,2] <- NaN

foo %<>%
  na.omit() %>% 
  mutate(score_groups = cut(scores,
                            breaks = c(-Inf, quantile(scores, c(1, 2)/3), Inf),
                            labels = c('low', 'medium', 'high')))

box_info <- group_by(foo, score_groups) %>% 
  summarize(first_quartile = quantile(scores, 0.25),
            third_quartile = quantile(scores, 0.75),
            iqr = IQR(scores),
            lower_bound = first_quartile - 1.5*IQR(scores),
            upper_bound = third_quartile + 1.5*IQR(scores))

foo %<>% inner_join(box_info, by="score_groups") %>% 
  mutate(is_outlier = scores > upper_bound | scores < lower_bound) %>% 
  select(ID, scores, score_groups, is_outlier)

foo %>% 
  ggplot(aes(y = scores, x=score_groups)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  geom_text(aes(label=ifelse(is_outlier, ID, '')), position=position_jitter()) + 
  theme_classic()
