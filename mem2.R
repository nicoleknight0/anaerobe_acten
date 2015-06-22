library(nlme)
library(lme4)

#look at datasets of pure cultures separately

purdata<-read.csv("analysis_data.csv")
purdata<-subset(purdata,Culture=="Pure")

purdata.sub<-purdata[,c("ID","Experiment","X1_kT","ln_rate")]

#calculate the activation energy for all the datasets of pure cultures

pur.acten<-groupedData(ln_rate~X1_kT|ID, purdata.sub, order.groups=FALSE)
test<-lmList(ln_rate~X1_kT|ID, data=purdata.sub, na.action=na.omit)
pairs(test, id=0.05)

#I'm comparing two linear models.  Both allow slope and intercept to vary with
#species (since we expect metabolic strategy to vary with species, of course)
#One allow both slope and intercept to vary with Experiment(pur.lm2), the other
#only allows intercept to vary (pur.lm1).

pur.lm1<-lmer(ln_rate~X1_kT+(1|Experiment)+(X1_kT|ID),data=purdata.sub)
pur.lm2<-lmer(ln_rate~X1_kT+(X1_kT|Experiment)+(X1_kT|ID),data=purdata.sub)

