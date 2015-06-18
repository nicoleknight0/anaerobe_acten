#mixed effects model to determine activation energies for anaerobic organisms

#library(nlme)
library(ggplot2)

mdata<-read.csv("analysis_data.csv")
pur_data<-subset(mdata,Culture="Pure")
mdata2<-pur_data[,c("Dataset","X1_kT","ln_rate")]




acten<-groupedData(ln_rate~X1_kT|Dataset, mdata2, order.groups=FALSE)
test<-lmList(ln_rate~X1_kT|Dataset, data=mdata2, na.action=na.omit)
pairs(test, id=0.05)

hist(coef(test)[,2], breaks=100, xlab='slope', main = 'Slopes from lmList individual model fitting')

#import coefficient data from above groupedData as csv, histogram of activation energies

cdata<-read.csv("coef_data2.csv")

hist_acten <-ggplot(cdata, aes(x=Activation_energy, fill=Order))
act_energies<-hist_acten + geom_bar()

#start with simple model (no random effects)

lm0<-lm(ln_rate~X1_kT,data=mdata)

#build mixed effects model (pure cultures only first)

pur_data<-subset(mdata,Culture="Pure")

lm1<-lme(ln_rate~X1_kT,data=pur_data,random=~1|Dataset)

plot(lm1,form=resid(.,type="p")~fitted(.)|Order,abline=0)

lm2<-lme(ln_rate~X1_kT,data=pur_data,random=~X1_kT|Dataset)

plot(lm2,form=resid(.,type="p")~fitted(.)|Order,abline=0)

#plot of each dataset with standard errors, coloured by Order

qplot(X1_kT,ln_rate,data=mdata,color=Order)+ geom_smooth(aes(group=Dataset), method="lm")
