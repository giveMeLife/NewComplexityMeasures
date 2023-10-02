knoise.par = seq(0,5, 0.5)
knoise.complexity = sapply(X= knoise.par, FUN=function(par) get.complexity(x=powernoise(k=par, N=10^4)[[1]], ndemb=6))

set.seed(123)
c <- c()
e <- c()
type <- c()
r <- c()

for(i in 1:100){
  n <- noise(kind = c("white"), samp.rate = 100)
  disc <- new_disc2(n@left, 5,4)
  comp <- MPR_complexity(disc)
  entropy <- permutation_entropy(disc)
  c <- c(c, comp)
  e <- c(e, entropy)
  type <- c(type, "new")
  r <- c(r, "white")
  
  
  n <- noise(kind = c("pink"), samp.rate = 100)
  disc <- new_disc2(n@left, 5,4)
  comp <- MPR_complexity(disc)
  entropy <- permutation_entropy(disc)
  e <- c(e, entropy)
  c <- c(c, comp)
  type <- c(type, "new")
  r <- c(r, "pink")
  
  
  n <- noise(kind = c("red"), samp.rate = 100)
  disc <- new_disc2(n@left, 5,4)
  comp <- MPR_complexity(disc)
  entropy <- permutation_entropy(disc)
  e <- c(e, entropy)
  c <- c(c, comp)
  type <- c(type, "new")
  r <- c(r, "red")
}

df <- data.frame(
  "type" = type,
  "complexity" = c,
  "entropy" = e,
  "noise" = r
)



plot(c(1,1), type='n', bty='n', ylim=c(0,0.6), xlim=c(0,1),
     xlab="Normalized Shannon Entropy", ylab = "MPR complexity")
lines(x=mind4[,1], y=mind4[,2])
lines(x=maxd4[,1], y=maxd4[,2])

points(x=df[df$noise=="white", ]$entropy, y=df[df$noise=="white", ]$complexity, col = "black")
points(x=df[df$noise=="pink", ]$entropy, y=df[df$noise=="pink", ]$complexity, col = "green")
points(x=df[df$noise=="red", ]$entropy, y=df[df$noise=="red", ]$complexity, col = "red")




ggboxplot(df, "noise", "entropy", color="noise")






c <- c()
e <- c()
type <- c()
r <- c()

for(i in 1:100){
  n <- noise(kind = c("white"), samp.rate = 100)
  disc <- ordinal_pattern_distribution(n@left,4)
  comp <- MPR_complexity(disc)
  entropy <- permutation_entropy(disc)
  c <- c(c, comp)
  e <- c(e, entropy)
  type <- c(type, "new")
  r <- c(r, "white")
  
  
  n <- noise(kind = c("pink"), samp.rate = 100)
  disc <- ordinal_pattern_distribution(n@left,4)
  comp <- MPR_complexity(disc)
  entropy <- permutation_entropy(disc)
  e <- c(e, entropy)
  c <- c(c, comp)
  type <- c(type, "new")
  r <- c(r, "pink")
  
  
  n <- noise(kind = c("red"), samp.rate = 100)
  disc <- ordinal_pattern_distribution(n@left,4)
  comp <- MPR_complexity(disc)
  entropy <- permutation_entropy(disc)
  e <- c(e, entropy)
  c <- c(c, comp)
  type <- c(type, "new")
  r <- c(r, "red")
}

df <- data.frame(
  "type" = type,
  "complexity" = c,
  "entropy" = e,
  "noise" = r
)


plot(c(1,1), type='n', bty='n', ylim=c(0,0.6), xlim=c(0,1),
     xlab="Normalized Shannon Entropy", ylab = "MPR complexity")
lines(x=mind4[,1], y=mind4[,2])
lines(x=maxd4[,1], y=maxd4[,2])

points(x=df[df$noise=="white", ]$entropy, y=df[df$noise=="white", ]$complexity, col = "black")
points(x=df[df$noise=="pink", ]$entropy, y=df[df$noise=="pink", ]$complexity, col = "green")
points(x=df[df$noise=="red", ]$entropy, y=df[df$noise=="red", ]$complexity, col = "red")