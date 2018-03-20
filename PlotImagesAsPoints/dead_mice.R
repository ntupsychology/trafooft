library(tidyverse)
library(ggimage)

Df = data.frame(x = 1:20,y = dbinom(1:20,20,.1), image = "image2.png")

pbinom(4,20,.1,lower.tail = FALSE)

Df %>% ggplot(aes(x,y)) +
  geom_line() + 
  geom_image(aes(image=image), size=.065) +
  geom_area(fill = "green", alpha = .3) +
  ylab("Probability") +
  xlab("Number of dead mice") +
  theme_light() 


ggsave("dead_mice.jpg",height = 8, width = 11, units = 'cm')



