library(arules)
library(statcomp)
library(tidyr)
library(tuneR)
library(ggpubr)
  
new_disc <- function(signal){
  combinations <- crossing(var1 = c(1,2,3), var2 = c(1,2,3), var3 = c(1,2,3))
  combinations$frequency <- rep(0, nrow(combinations))
  
  sig_disc <- discretize(signal, "interval", breaks = 3)
  sig_disc <- as.numeric(sig_disc)
  for(i in 1:(length(sig_disc)-2)){
    combinations[combinations$var1 == sig_disc[i] & combinations$var2 == sig_disc[i+1] & combinations$var3 == sig_disc[i+2],]$frequency = combinations[combinations$var1 == sig_disc[i] & combinations$var2 == sig_disc[i+1] & combinations$var3 == sig_disc[i+2],]$frequency + 1
  }
  return(combinations$frequency)
}

new_disc2 <- function(signal, n, embdim){
 
  sig_disc <- discretize(signal, "interval", breaks = n)
  sig_disc <- as.numeric(sig_disc)
  opd <- ordinal_pattern_distribution(sig_disc, embdim)
  return(opd)
}




