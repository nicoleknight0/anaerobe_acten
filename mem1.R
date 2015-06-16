#mixed effects model to determine activation energies for anaerobic organisms

library(nlme)

mdata<-read.csv("analysis_data.csv")
mdata2<-mdata[,c("Dataset","X1_kT","ln_rate")]

acten<-groupedData(ln_rate~X1_kT|Dataset, mdata2, order.groups=FALSE)
test<-lmList(ln_rate~X1_kT|Dataset, data=mdata2, na.action=na.omit)
pairs(test, id=0.05)

hist(coef(test)[,2], breaks=100, xlab='slope', main = 'Slopes from lmList individual model fitting')

#import coefficient data from above groupedData as csv

cdata<-read.csv("coef_data.csv")

hist(coef(test)[,2],breaks=30,xlab="Ea")
