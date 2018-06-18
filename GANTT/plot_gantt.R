library(tidyverse)
library(yaml)

# This reads in data from a yaml file. Yaml file are just text files. In this case 
# it needs to look like this...
# 
# - phase:	Planning / Coordination
# task:	Coordination and progress monitoring
# start:	2018-09-01
# end:	2020-08-31
# - phase:	Integration / dissemination
# task:	Maintain shared workspace
# start:	2018-09-01
# end:	2020-08-31
# - phase:	Planning / Coordination
# task:	All-team Meeting
# start:	2018-11-07
# end:	2018-11-11

## get data from yaml file
gantt_data_yaml_file = 'EFG_gantt.yml'

m = as.matrix(unlist(read_yaml(gantt_data_yaml_file)))
vars = unique(row.names(m))
Df = as.tibble(matrix(m,length(m)/length(vars),length(vars),byrow = T))
names(Df) = vars
 
if (!(all(all(vars == c('phase','task','start','end')),length(vars == 4)))){
  message("\nError: yaml must have just these fields: phase, task, start, end\n")
}

Df = Df %>% gather(start_end,time,-phase,-task) %>%
  mutate(time = as.Date(time),
         phase = factor(phase, levels = unique(phase)),
         task = factor(task, levels = rev(unique(task))))

mytheme = theme_light(base_size = 10) +
          theme(plot.title = element_blank(),
          panel.grid.major.x = element_line(colour="black", linetype = "dashed"),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 0),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_blank())

Df %>% ggplot(aes(time, task, colour = phase)) + 
  geom_line(size = 6) +
  scale_x_date(date_labels = "%Y %b", limits = c(min(Df$time), NA), date_breaks = '4 months', expand = c(.01,.01)) +
  mytheme

ggsave('gantt.pdf',width = 20, height = 10.2, units = 'cm')
ggsave('gantt.png',width = 20, height = 10.2, units = 'cm')
