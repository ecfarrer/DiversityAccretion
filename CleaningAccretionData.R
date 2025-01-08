# Loading, cleaning, calculating accretion data

library(tidyverse)
library(ggplot2)
#library(plotrix)
#library(vegan)
#library(nlme)
#library(chron)


##### General cleaning and averaging  ######
acc<-read.csv("Data/CRMS_Accretion.csv",stringsAsFactors = F)
acc$Station.ID<-factor(acc$Station.ID)
acc$Group<-factor(acc$Group)
head(acc)
levels(acc$Group)

#The station ID is unique to a core that was inserted on one particular date and then extracted on multiple other dates. The Group refers to the year that the core was started, PS1 is 2008, PS2 is 2010, PS3 is 2012, PS4 is 2014, PS5 is 2016, PS6 is 2018, PS7 is 2020, PS8 is 2023. There are multiple cores started in for example in 2008 at a given station front (CRMS0002)

#Sometimes multiple cores were taken on a particular date at a particular stationID b/c the first core was poor for example, but many have at least one measurement for multiple cores if they were taken out on the same date. average the 4 measurements per core first, then average over the two or more cores at each sampling time. Then delete all the NaN values (due to cores being taken but being bad and no measurements were written)
acc2<-acc%>%
  mutate(accmm1=rowMeans(dplyr::select(acc, starts_with("Accretion")), na.rm = TRUE))%>%
  mutate(StationFront=Station.ID)%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="-")%>%
  group_by(Station.ID,StationFront,StationBack,Group, Sample.Date..mm.dd.yyyy.,Establishment.Date..mm.dd.yyyy.)%>%
  summarise(accmm=mean(accmm1,na.rm=T))%>%
  group_by(Station.ID)%>%
  arrange(mdy(Sample.Date..mm.dd.yyyy.),.by_group=TRUE)%>%
  filter(is.na(accmm)==F)%>%
  rename(sampledate=Sample.Date..mm.dd.yyyy.,estabdate=Establishment.Date..mm.dd.yyyy.)

head(as.data.frame(acc2))

#filter(!(Station.ID=="CRMS0392-A05"&Sample.Date..mm.dd.yyyy.=="2/11/2009"))#delete things that look odd (i think they recorded it in cm not mm): CRMS0392-A05 in 2009. I'm leaving this in for now since it doesn't look too off compared to the future other values. it could be real if there was erosion
# temp<-as.data.frame(acc2%>%
#   filter(Station.ID=="CRMS0392-A05")%>%
#   mutate(year=year(mdy(Sample.Date..mm.dd.yyyy.))))
# plot(temp$year,temp$accmm)

dim(acc2) #53208 x 7
as.data.frame(acc2)[1:15,]





##### Trying to replicate Jankowski et al 2017 numbers #####

#Using methods from Jankowski et al 2017, they averaged the measurements from one core, then averaged the measurements from all cores taken at one time period. Then did regressions (using a slope and intercept, and not including a 0,0 point). These were my original notes from when I did this in 2017. Looking at her methods again, I can't find anywhere saying whether she averaged per core first and then all cores. In her methods she just says "A mean of the 12 VA measurements was calculated after each site visit." which sounds like she just averaged everything at once.

#Trying it out for the plot (CRMS0549) Jankowski shows in the supplement. it looks like Jankowski only used PS1, I don't see why we shouldn't use all cores regardless of when they were initiated
test<-acc2%>%
  filter(StationFront=="CRMS0549")%>%# CRMS0549 CRMS0174
  mutate(days=mdy(sampledate)-mdy(estabdate),years=days/365)%>%
  group_by(Group,sampledate,days,years)%>%
  summarise(meanaccmm=mean(accmm))%>%
  group_by(Group)%>%
  arrange(mdy(sampledate),.by_group=TRUE)%>%
  filter(years>0,Group=="PS1")%>% # Group%in%c("PS3","PS4")
  filter(years<9)%>%
  arrange(years)

test<-acc2%>%
  filter(StationFront=="CRMS0549",Group=="PS1")# CRMS0549 CRMS0174
View(test)  


#I can't seem to replicate the Jankowski numbers exactly. I will try (below) just averaging all the numbers (not averaging by plot first). This makes the numbers closer (but still not exactly like what Jankowski got in the supplement). For example, the number for 4/23/2007 is very similar now, but the number for 10/30/2007 is different (for J it is less than 4/23, for me it is greater.) In general it is just the first two dates whose points I can't get to agree, the later dates look pretty darn close.

test<-acc%>%
  mutate(StationFront=Station.ID)%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="-")%>%
  filter(StationFront=="CRMS0549",Group=="PS1")%>%
  pivot_longer(Accretion.Measurement.1..mm.: Accretion.Measurement.4..mm.,names_to = "rep",values_to = "acc1")%>%
  arrange(mdy(Sample.Date..mm.dd.yyyy.))%>%
  group_by(Sample.Date..mm.dd.yyyy.,Establishment.Date..mm.dd.yyyy.)%>%
  summarise(meanaccmm=mean(acc1,na.rm=T))%>%
  mutate(days=mdy(Sample.Date..mm.dd.yyyy.)-mdy(Establishment.Date..mm.dd.yyyy.),years=days/365)%>%
  arrange(years)%>%
  filter(years<9&years>.1)


as.data.frame(test)
test<-as.data.frame(test)
plot(test$years,test$meanaccmm, ylim=c(0,200),xlim=c(0,9))
abline(lm(test$meanaccmm~test$years))
coef(lm(meanaccmm~years,data=test))[2]
lm(meanaccmm~years,data=test)
summary(lm(meanaccmm~years,data=test))

#The upshot is that I can't recreate the Jankowski numbers exactly, but I'm pretty darn close - for the site above I get 22.45 mm/yr with R2 .97, and she gets 22.53 mm/yr with R2 .97. So I will proceed with the understanding that I am doing things correctly. I decided to average over the core first and then average the cores, since I think this is a better way of doing things.



##### Playing with all data for some of the plots to make sure that with the length of the dataset being much longer than Jankowski, the regressions still make sense
test<-acc2%>%
  filter(StationFront=="CRMS2854")%>%#  CRMS0174 CRMS0549 CRMS0002 CRMS0033 CRMS0030 CRMS2854 CRMS0311
  mutate(days=mdy(sampledate)-mdy(estabdate),years=days/365)%>%
  group_by(Group,sampledate,days,years)%>%
  summarise(meanaccmm=mean(accmm))%>%
  group_by(Group)%>%
  arrange(mdy(sampledate),.by_group=TRUE)%>%
  filter(years>0,Group%in%c("PS1"))%>% # ,Group%in%c("PS3","PS4")
  arrange(years)
as.data.frame(test)
test<-as.data.frame(test)
plot(test$years,test$meanaccmm)
abline(lm(test$meanaccmm~test$years))
coef(lm(meanaccmm~years,data=test))[2]
#lm(meanaccmm~years,data=test)
summary(lm(meanaccmm~years,data=test))

#for some sites (e.g. CRMS0033) PS2 is actually longer than PS1, so I might have to specify a start date and end date and just use sites in that range
#I like using just one starting point (i.e. PS1 OR PS2). PS1 and PS2 aren't THAT different from one another but if you use too many like PS4 might only have 4 years of measurements and then you just get a lot of points clustered around the origin (low years values)
#I'm not sure if I could get 2 accretion rates out of these data (for the first 6 years and last 6 years) since there would only be about 5-6 points for the last 6 years which doesn't make a great n for a regression line. However I could do moving window analysis or something??





#Get histogram of maximum length of time for the sites. Jankowski used sites with >=6 yrs of data
head(data.frame(acc2))
test<-acc2%>%
  mutate(days=mdy(sampledate)-mdy(estabdate),years=days/365)%>%
  group_by(StationFront)%>%
  summarise(maxyrs=max(years))
head(data.frame(test))
hist(as.numeric(test$maxyrs),breaks=16)
sort(as.numeric(test$maxyrs))
length(which(test$maxyrs>13))
#There should be 235 plots with >13 years of data

test<-acc2%>%
  filter(year(mdy(estabdate))==2007)%>%
  filter(year(mdy(sampledate))==2020)%>%
  group_by(StationFront,estabdate,sampledate)%>%
  summarise(meanacc=mean(accmm))
#data.frame(test)
dim(test)
#2008-2022: 71 sites
#2008-2020: 72
#2008-2018: 75
#2007-2020: 59

#Better b/c some might have been started earlier than 2008 so we are just looking for a sampling date in 2008
test1<-acc2%>%
  filter(year(mdy(sampledate))==2007)%>%
  group_by(StationFront,estabdate,sampledate)%>%
  summarise(meanacc=mean(accmm))
test2<-acc2%>%
  filter(year(mdy(sampledate))==2020)%>%
  group_by(StationFront,estabdate,sampledate)%>%
  summarise(meanacc=mean(accmm))
length(intersect(test1$StationFront,test2$StationFront))
#2007-2020: 180 sites
#2008-2018: 241 sites






##### haven't done below yet ####
##### Calculating one accretion rate per plot via regression ####

#Doing calcs on all plots
#after checking the data, I filtered plots that had fewer than 5 measurements, since a slope on n=4 is sketchy. this removed the negative slopes I was getting.
acc2a<-acc2%>%
  mutate(days=mdy(sampledate)-mdy(estabdate),years=days/365)%>%
  group_by(StationFront,Group,sampledate,days,years)%>%
  summarise(meanaccmm=mean(accmm))%>%
  filter(meanaccmm>0)%>%
  group_by(StationFront)%>%
  summarise(acc=coef(lm(meanaccmm~years))[2],n=n())%>%
  filter(n>4)
as.data.frame(acc2a)[1:30,]

#check that it is same as above, yes
acc2a[acc2a$StationFront=="CRMS0549",]
sort(acc2a$acc)
acc2a[which(acc2a$acc<0),]
acc2a[which(acc2a$acc>100),]
# the plot CRMS0174 has a value over 100mm/yr, this looks actually correct based on the data, although there is a note in the raw data file that one measurement was affected by a large storm deposit, so I could delete if it looks really odd. there is no veg data from this station







