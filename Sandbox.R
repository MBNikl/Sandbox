#-------------------------
# Power Analysis
#-------------------------

library(pwr)
pwr.t.test(n=6, sig.level=0.05, power=0.8, type='two.sample')

#-------------------------
# YNSKE code
#-------------------------
x  <- NULL
y  <- NULL


for (i in 1:10000000) {
     x  <- paste(sample(letters,5), collapse='')
     y[i] <- as.data.frame(table(x=='ynske'))[,2] 
}
#-------------------------
# tapply and with 
#-------------------------
# tapply example
tapply(iris$Sepal.Length, iris$Species, mean)
with(iris, tapply(Sepal.Length, Species, mean))

#----------------------------
# R output into Latex-table
#----------------------------
library(xtable)
library(VGAM)
data(hspider) # the data is now in the hspider object
str(hspider)
hs.ab.raw<-as.matrix(hspider[,7:18])
xtable(hspider)
