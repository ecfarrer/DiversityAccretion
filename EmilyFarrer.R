library(tidyverse)
library(piecewiseSEM)
library(sjstats) #contains r2(), doesn't work
library(partR2) #contains partR2(), doesn't work
library(MuMIn) #doesnt work for gls

head(dat)

options(contrasts=c("contr.helmert","contr.poly"))

dat1<-dat%>%
  filter(CommunityStationFront!="Swamp")%>%
  mutate(eastwest=ifelse(lon<(-92),"west","east"))

m1<-gls(SummedCover~CommunityStationFront*eastwest,correlation=corSpher(form = ~ lat+lon),data=dat1)

anova(m1,type="marginal")

ggplot(dat1,aes(y=SummedCover,color=CommunityStationFront))+
  geom_boxplot()+
  facet_wrap(~eastwest)

rsquared(m1)
#r2(m1)
#partR2(m1)
#r.squaredGLMM(m1)

SSR<-sum((residuals(m1,type="response")^2))
SST<-sum((dat1$SummedCover - mean(dat1$SummedCover,na.rm=T))^2,na.rm=T)
R2<-1-SSR/SST
# n<-sum(!is.na(dat1$SummedCover))
# k<-5 #(number of fixed effects parameters)
# 1-((1 - R2) * (n - 1)/ (n - k - 1))
# m1<-lm(SummedCover~Substrate*Inoculum, data=dat1,na.action = na.omit)
# summary(m1)


#Testing step() on gls() model

library(MASS)
m1<-gls(SummedCover~CommunityStationFront*eastwest,correlation=corSpher(form = ~ lat+lon),data=dat1,method="ML")
#m0 <- gls(SummedCover ~ 1,correlation=corSpher(form = ~ lat+lon),data=dat1,method="ML")

mf<-stepAIC(m1,direction="backward")
anova(mf)

