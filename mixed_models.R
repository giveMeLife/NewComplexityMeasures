library(lme4)
library(nlme)
library(emmeans)
library(merTools)

data <- read.csv('results.csv') 

subdata <- data[data$signal == 'RCBFV' & data$group == 'HC', ]

subdata$phase <- as.factor(subdata$phase)

mixto <- lmer(complexity ~ phase + (1|subject), data = subdata )
medias <- emmeans (mixto , "phase")
post.hoc <- pairs (medias , adjust = "tukey")
print(post.hoc)

