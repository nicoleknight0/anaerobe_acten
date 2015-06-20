(#mixed effects model to determine activation energies for anaerobic organisms

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

pur_cdata<-subset(cdata,Order!="Mixed")

hist_acten_pur <-ggplot(pur_cdata, aes(x=Activation_energy, fill=Order))
act_energies<-hist_acten_pur + geom_bar()


#start with simple model (no random effects)

lm0<-lm(ln_rate~X1_kT,data=mdata)

#build mixed effects model (pure cultures only first)

pur_data<-subset(mdata,Culture=="Pure")

lm1<-lme(ln_rate~X1_kT,data=pur_data,random=~1|Dataset)

plot(lm1,form=resid(.,type="p")~fitted(.)|Order,abline=0)

lm2<-lme(ln_rate~X1_kT,data=pur_data,random=~X1_kT|Dataset)

plot(lm2,form=resid(.,type="p")~fitted(.)|Order,abline=0)

#plot of each dataset with standard errors, coloured by Order

library(lme4)
#lets try running some tests ony using pure culture data and specific growth rates

subdata<-subset(mdata,Culture=="Pure")
subdata<-subset(subdata,Rate_type=="Specific growth")

subdata2<-subdata[,c("Dataset","X1_kT","ln_rate")]


pur_lm<-lmer(ln_rate~X1_kT+(1+X1_kT|Dataset),data=subdata2)
#cint_pur<-confint(pur_lm)

acten_subdata<-groupedData(ln_rate~X1_kT|Dataset, subdata2, order.groups=FALSE)
test_subdata<-lmList(ln_rate~X1_kT|Dataset, data=subdata2, na.action=na.omit)
pairs(test_subdata, id=0.05)

hist(coef(test_subdata)[,2], breaks=50, xlab='slope', main = 'Slopes from lmList individual model fitting')


mix_data<-subset(mdata,Culture=="Mixed")

mix_data2<-mix_data[,c("Dataset","X1_kT","ln_rate")]
mix_lm<- lmer(ln_rate~X1_kT + (1+X1_kT|Dataset), data = mix_data)
#cint_mix<-confint(mix_lm)

#see if activation energies of pure and mixed cultures are significantly different

coef_data<-read.csv("coef_data2.csv")

an.1<-aov(Activation_energy~Culture,data=coef_data)


hist_acten_cult <-ggplot(coef_data, aes(x=Activation_energy, fill=Type))
cult_act_energies<-hist_acten_cult + geom_bar()