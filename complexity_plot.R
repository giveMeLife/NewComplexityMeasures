library(ggpubr)
library(ggplot2)
library(statcomp)

limits_low <- data.frame('xmin' = mind4[,1],
                     'ymin' = mind4[,2]) 

limits_max <- data.frame('xmax' = maxd4[,1],
                         'ymax' = maxd4[,2])

data <- read.csv('results.csv') 

subdata <- data[data$signal == 'LCBFV' & data$group == 'OFF', ]

ggscatter(subdata, 'entropy', 'complexity', color = 'phase') + 
  geom_line(data = limits_low, aes(x = xmin, y =ymin)) +
  geom_line(data = limits_max, aes(x = xmax, y =ymax))
  

