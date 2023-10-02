source("new_permutation.R")


data <- read.csv("data_paired_fixed_length.csv")


hemispheres <- c("LCBFV", "RCBFV")
groups <- c("HC", "ON", 'OFF')
phases <- c("phase1", "phase2", "phase3")


subjects <- unique(data$subject)

h <- c()
g <- c()
p <- c()
s <- c()
c <- c()
e <- c()

for(hemisphere in hemispheres){
  print("#############################")
  print(hemisphere)
  for(group in groups){
    print(group)
    for(phase in phases){
      print(phase)
      for(subject in subjects){
        subdata <- data[data$subject == subject & data$phase == phase & data$type == hemisphere & data$group == group, ]
        if(nrow(subdata)!= 0){
          signal <- subdata$signal
          opd <- new_disc(signal)
          complexity <- MPR_complexity(opd)
          entropy <- permutation_entropy(opd)
          h <- c(h, hemisphere)
          g <- c(g, group)
          p <- c(p, phase)
          s <- c(s, subject)
          c <- c(c, complexity)
          e <- c(e, entropy)
        }
        
      }
    }
  }
}


df <- data.frame(
  "signal" = h,
  "group" = g,
  "phase" = p,
  "subject" = s,
  "complexity" = c,
  "entropy" = e
)


write.csv(df, "results.csv")



subdf <- df[df$signal == "RCBFV" & df$group == "OFF", ]

ggscatter(subdf, "entropy", "complexity", color = "phase")


a <- data[data$type =="RCBFV" & data$group == "HC" & data$phase == "phase1" & data$subject == "S-6", ]
b <- data[data$type =="RCBFV" & data$group == "HC" & data$phase == "phase1" & data$subject == "S-10", ]