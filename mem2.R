library(nlme)
library(lme4)

#look at datasets of pure cultures separately

an.data<-read.csv("analysis_data.csv")
purdata<-subset(an.data,Culture=="Pure")
mixdata<-subset(an.data,Culture=="Mixed")

purdata.sub<-purdata[,c("ID","Experiment","Dataset","X1_kT","ln_rate","pH","Order")]
mixdata.sub<-mixdata[,c("Experiment","Dataset","X1_kT","ln_rate","pH")]

#calculate the activation energy for all the datasets of pure cultures

pur.acten<-groupedData(ln_rate~X1_kT|Dataset, purdata.sub, order.groups=FALSE)
test<-lmList(ln_rate~X1_kT|Dataset, data=purdata.sub, na.action=na.omit)
pairs(test, id=0.05)

#now mixed

mix.acten<-groupedData(ln_rate~X1_kT|Dataset, mixdata.sub, order.groups=FALSE)
test<-lmList(ln_rate~X1_kT|Dataset, data=mixdata.sub, na.action=na.omit)
pairs(test, id=0.05)

#Linear models for the pure cultures.
#All except the first (pur.lm1) allow slope and intercept to vary with
#species (since we expect metabolic strategy to vary with species, of course)
#One allow both slope and intercept to vary with Experiment(pur.lm2), the other
#only allows intercept to vary (pur.lm1).
#best so far: pur.lm3

pur.lm0<-lmer(ln_rate~X1_kT+(1|Experiment),data=purdata.sub)
pur.lm0b<-lmer(ln_rate~X1_kT+(X1_kT|Experiment),data=purdata.sub) # pur.lm0 vs pur.lm0b tests whether diff slopes are req.
pur.lm1<-lmer(ln_rate~X1_kT+(1|Experiment)+(1|ID),data=purdata.sub)
pur.lm1b<-lmer(ln_rate~X1_kT+(1|Experiment)+(X1_kT|ID),data=purdata.sub) # pur.lm1 vs pur.lm1b tests whether diff slopes are req.
pur.lm2<-lmer(ln_rate~X1_kT+(1|Experiment)+(1|ID)+(1|Dataset),data=purdata.sub)
pur.lm2b<-lmer(ln_rate~X1_kT+(1|Experiment)+(1|ID)+(X1_kT|Dataset),data=purdata.sub)

pur.lm3<-lmer(ln_rate~X1_kT+(1|Experiment)+(1|ID)+(X1_kT|Dataset)+(1|Order),data=purdata.sub)
pur.lm3b<-lmer(ln_rate~X1_kT+(1|Experiment)+(1|ID)+(X1_kT|Dataset)+(X1_kT|Order),data=purdata.sub,control=lmerControl(optimizer=bobyqa,optctrl = list(maxfun = 100000)))

#This model looks close. correlation between random effects is relatively low (could be lower). 
# next steps: create a variable that collapses Rate_tupe into as few categories as possible. Maybe metabolite production rates, doublings, and consumption rates? then, a different column for substrates or metabolites (lactate, acetate, nitrate, etc?)
pur.lm4 <- lmer(ln_rate ~ I(X1_kT-38) + (1|Dataset) + (1 + I(X1_kT-38)|Order), data=purdata.sub)

head(purdata.sub)
