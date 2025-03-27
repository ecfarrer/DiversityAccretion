# Loading, cleaning, calculating accretion data


##### General cleaning and averaging  ######
acc<-read.csv("Data/CRMS_Accretion.csv",stringsAsFactors = F)
acc$Station.ID<-factor(acc$Station.ID)
acc$Group<-factor(acc$Group)
head(acc)
levels(acc$Group)

#The station ID is unique to a core that was inserted on one particular date and then extracted on multiple other dates. The Group refers to the year that the core was started, PS1 is 2008, PS2 is 2010, PS3 is 2012, PS4 is 2014, PS5 is 2016, PS6 is 2018, PS7 is 2020, PS8 is 2023. There are multiple cores started in for example in 2008 at a given station front (CRMS0002)

#Sometimes multiple cores were taken on a particular date at a particular stationID b/c the first core was poor for example, but many have at least one measurement for multiple cores if they were taken out on the same date. average the 4 measurements per core first, then average over the two or more cores at each sampling time. Then delete all the NaN values (due to cores being taken but being bad and no measurements were written)
#this was the old way of doing things (better I think, but not what Jankowsi did)
# acc2<-acc%>%
#   rename(StationID = Station.ID)%>%
#   mutate(accmm1=rowMeans(dplyr::select(acc, starts_with("Accretion")), na.rm = TRUE))%>%
#   mutate(StationFront=StationID)%>%
#   separate(StationFront,into=c("StationFront","StationBack"),sep="-")%>%
#   group_by(StationID,StationFront,StationBack,Group, Sample.Date..mm.dd.yyyy.,Establishment.Date..mm.dd.yyyy.)%>%
#   summarise(accmm=mean(accmm1,na.rm=T))%>%
#   group_by(StationID)%>%
#   arrange(mdy(Sample.Date..mm.dd.yyyy.),.by_group=TRUE)%>%
#   filter(is.na(accmm)==F)%>%
#   rename(sampledate=Sample.Date..mm.dd.yyyy.,estabdate=Establishment.Date..mm.dd.yyyy.)
# 
# head(as.data.frame(acc2))
# dim(acc2) #53208 x 7
# as.data.frame(acc2)[1:15,]


#This is averaging over all the values (not averaging per core first)
acc2<-acc%>%
  rename(StationID = Station.ID,sampledate=Sample.Date..mm.dd.yyyy.,estabdate=Establishment.Date..mm.dd.yyyy.)%>%
  mutate(StationFront=StationID)%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="-")%>%
  pivot_longer(Accretion.Measurement.1..mm.: Accretion.Measurement.4..mm.,names_to = "rep",values_to = "acc1")%>%
  group_by(StationFront,Group,sampledate,estabdate)%>% 
  summarise(meanaccmm=mean(acc1,na.rm=T),lat=mean(Latitude),lon=mean(Longitude))%>%  
  arrange(StationFront,Group,mdy(sampledate))%>%
  mutate(days=mdy(sampledate)-mdy(estabdate),years=days/365)%>%
  filter(!is.na(meanaccmm),years>.01)
  
data.frame(acc2)
View(acc2)
head(as.data.frame(acc2))

dim(acc2)
#18212

#filter(!(StationID=="CRMS0392-A05"&Sample.Date..mm.dd.yyyy.=="2/11/2009"))#delete things that look odd (i think they recorded it in cm not mm): CRMS0392-A05 in 2009. I'm leaving this in for now since it doesn't look too off compared to the future other values. it could be real if there was erosion
# temp<-as.data.frame(acc2%>%
#   filter(StationID=="CRMS0392-A05")%>%
#   mutate(year=year(mdy(Sample.Date..mm.dd.yyyy.))))
# plot(temp$year,temp$accmm)




##### Trying to replicate Jankowski et al 2017 numbers #####

#Using methods from Jankowski et al 2017, "A mean of the 12 VA measurements was calculated after each site visit.". Then did regressions (using a slope and intercept, and not including a 0,0 point). 

#Trying it out for the plot (CRMS0549) Jankowski shows in the supplement. Jankowski only used PS1, I don't see why we shouldn't use all cores regardless of when they were initiated
#note this uses the first acc2 which average within core first and then within StationID and now here within StationFront
# test<-acc2%>%
#   filter(StationFront=="CRMS0549")%>%# CRMS0549 CRMS0174
#   mutate(days=mdy(sampledate)-mdy(estabdate),years=days/365)%>%
#   group_by(Group,sampledate,days,years)%>%
#   summarise(meanaccmm=mean(accmm))%>%
#   group_by(Group)%>%
#   arrange(mdy(sampledate),.by_group=TRUE)%>%
#   filter(years>0,Group=="PS1")%>% # Group%in%c("PS3","PS4")
#   filter(years<9)%>%
#   arrange(years)
# 
# test<-acc2%>%
#   filter(StationFront=="CRMS0549",Group=="PS1")# CRMS0549 CRMS0174
# View(test)  


#I can't seem to replicate the Jankowski numbers exactly. I will try (below) just averaging all the numbers (not averaging by plot first). This makes the numbers closer (but still not exactly like what Jankowski got in the supplement). For example, the number for 4/23/2007 is very similar now, but the number for 10/30/2007 is different (for J it is less than 4/23, for me it is greater.) In general it is just the first two dates whose points I can't get to agree, the later dates look pretty darn close.

test<-acc%>%
  rename(StationID = Station.ID)%>%
  mutate(StationFront=StationID)%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="-")%>%
  filter(StationFront=="CRMS0549",Group=="PS1")%>% # CRMS0605
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
mod1<-lm(meanaccmm~years,data=test)
summary(lm(meanaccmm~years,data=test))
sqrt(mean(mod1$residuals^2))

RSS <- c(crossprod(mod1$residuals))
MSE <- RSS / length(mod1$residuals)
RMSE <- sqrt(MSE)

#check with new acc2
test2<-acc2%>%
  filter(StationFront=="CRMS0549",Group=="PS1")
test2
#yes checks

#The upshot is that I can't recreate the Jankowski numbers exactly, but I'm pretty darn close - for the site above I get 22.46 mm/yr with R2 .97, and she gets 22.53 mm/yr with R2 .97. So I will proceed with the understanding that I am doing things correctly. in email, Jankowski agreed that these numbers were so close to be indistinguishable and that it looked fine and that the slight differences might be due to rounding error. I think averaging over the core first and then average the cores is better BUT I will do it Jankowsi way so that I can cite her methods exactly.


##### Playing with all data for some of the plots to make sure that with the length of the dataset being much longer than Jankowski, the regressions still make sense

#I sent this first figure to Jankowski so I'm leaving the code here untouched
#note this is done with the original acc2
# test<-acc2%>%
#   filter(StationFront=="CRMS0549")%>%#  CRMS2854 CRMS0174  CRMS0002 CRMS0033 CRMS0030 CRMS2854 CRMS0311
#   mutate(days=mdy(sampledate)-mdy(estabdate),years=days/365)%>%
#   group_by(Group,sampledate,days,years)%>%
#   summarise(meanaccmm=mean(accmm))%>%
#   group_by(Group)%>%
#   arrange(mdy(sampledate),.by_group=TRUE)%>%
#   filter(years>0,Group%in%c("PS1"))%>% # ,Group%in%c("PS3","PS4")
#   arrange(years)
# as.data.frame(test)
# test<-as.data.frame(test)
# plot(test$years[1:10],test$meanaccmm[1:10],pch=16,col="black",xlim=c(0,18),ylim=c(0,200),ylab="Accretion (mm)",xlab="Year")
# points(test$years[11:14],test$meanaccmm[11:14],pch=16,col="red")
# abline(lm(test$meanaccmm~test$years))
# coef(lm(meanaccmm~years,data=test))[2]
# #lm(meanaccmm~years,data=test)
# summary(lm(meanaccmm~years,data=test))


#Messing with things below
#using the original acc2
# test<-acc2%>%
#   filter(StationFront=="CRMS2854")%>%#  CRMS0549 CRMS2854 CRMS0174  CRMS0002 CRMS0033 CRMS0030 CRMS2854 CRMS0311
#   mutate(days=mdy(sampledate)-mdy(estabdate),years=days/365)%>%
#   group_by(Group,sampledate,days,years)%>%
#   summarise(meanaccmm=mean(accmm))%>%
#   group_by(Group)%>%
#   arrange(mdy(sampledate),.by_group=TRUE)%>%
#   filter(years>0,Group%in%c("PS1","PS2","PS3"))%>% # ,Group%in%c("PS3","PS4")
#   arrange(Group)
# as.data.frame(test)
# test<-as.data.frame(test)
# ggplot(test,aes(x=years,y=meanaccmm,color=Group))+#Accretion (mm)
#   geom_point(aes())+
#   geom_smooth(method="lm",se=F)
# coef(lm(meanaccmm~years,data=test))[2]
# #lm(meanaccmm~years,data=test)
# summary(lm(meanaccmm~years,data=test))

#using new acc2
test2<-acc2%>%
  filter(StationFront=="CRMS0549")%>%
  filter(Group%in%c("PS1","PS2","PS3"))%>% # ,Group%in%c("PS3","PS4")
  arrange(Group)
test2<-as.data.frame(test2)
ggplot(test2,aes(x=years,y=meanaccmm,color=Group))+#Accretion (mm)
  geom_point(aes())+
  geom_smooth(method="lm",se=F)
coef(lm(meanaccmm~years,data=test2))[2]
#lm(meanaccmm~years,data=test)
summary(lm(meanaccmm~years,data=test2))
#the points are very slightly different but the patterns, coef, R2 are very similar using the new method of just averaging over the 12 measurements

#for some sites (e.g. CRMS0033) PS2 is actually longer than PS1, so I might have to specify a start date and end date and just use sites in that range
#I like using just one starting point (i.e. PS1 OR PS2). PS1 and PS2 aren't THAT different from one another but if you use too many like PS4 might only have 4 years of measurements and then you just get a lot of points clustered around the origin (low years values). even PS1, PS2 and PS3 are all pretty long and the slopes are remarkably similar to each other for some plots (not for CRMS0549 though).
#I'm not sure if I could get 2 accretion rates out of these data (for the first 6 years and last 6 years) since there would only be about 5-6 points for the last 6 years which doesn't make a great n for a regression line. However I could do moving window analysis or something??
#for right now, initial analysis, I'll just use PS1 and one single slope over the entire period




#Get histogram of maximum length of time for the sites. Jankowski used sites with >=6 yrs of data
head(data.frame(acc2))
test<-acc2%>%
  mutate(days=mdy(sampledate)-mdy(estabdate))%>%
  group_by(StationFront)%>%
  summarise(maxyrs=max(years))
head(data.frame(test))
hist(as.numeric(test$maxyrs),breaks=16)
sort(as.numeric(test$maxyrs))
length(which(test$maxyrs>13))
#There should be 235 sites with >13 years of data

test<-acc2%>%
  filter(year(mdy(estabdate))==2007)%>%
  filter(year(mdy(sampledate))==2020)
  # group_by(StationFront,estabdate,sampledate)%>%
  # summarise(meanacc=mean(accmm))
dim(test)
#2008-2022: 71 sites
#2008-2020: 72
#2008-2018: 75
#2007-2020: 59

#Better b/c some might have been started earlier than 2008 so we are just looking for a sampling date in 2008
test1<-acc2%>%
  filter((year(mdy(estabdate))==2007&year(mdy(sampledate))==2020)|(year(mdy(estabdate))==2008&year(mdy(sampledate))==2020)|(year(mdy(estabdate))==2007&year(mdy(sampledate))==2018)|(year(mdy(estabdate))==2008&year(mdy(sampledate))==2018)|(year(mdy(estabdate))==2006&year(mdy(sampledate))==2020)|(year(mdy(estabdate))==2006&year(mdy(sampledate))==2020))
  # group_by(StationFront)%>%
  # summarise(meanacc=mean(accmm))
dim(test1)
#not sure if this is correct, revisit if needed
#146 2007/2008 to 2018/2020 
#197 2006/2007/2008 to 2018/2020 


#histogram of when PS1 starts
test<-acc2%>%
  filter(Group=="PS1")
unique(year(mdy(test$estabdate)))
#all PS1s start in 2006, 2007, 2008, or 2009. That is pretty close together.
#histogram of when PS1 ends
test<-acc2%>%
  filter(Group=="PS1")%>%
  group_by(StationFront)%>%
  summarize(max=max(mdy(sampledate)))
unique(year(ymd(test$max)))
hist(year(ymd(test$max)))
sort(year(ymd(test$max)))
#there is a range of when PS1 ends, but the majority are 2020 and 2022, some from 2018. could do 2020, 2021, 2022, 2023


#Filter plots that have beginning date 2006-2009 (all of them) and end date 2020-2023
acc3<-acc2%>%
  filter(Group=="PS1")%>%
  group_by(StationFront)%>%
  mutate(year=year(mdy(sampledate)))%>%
  filter(any(year %in% c(2020,2021,2022,2023))) %>%
  ungroup()%>%
  filter(year<2024)
dim(acc3)
length(unique(acc3$StationFront))
data.frame(acc3)
#259 sites

#check that all have >=6 time points, no. delete one site with n=4. this will be done below in the regressions
# test<-acc3%>%
#   group_by(StationFront)%>%
#   summarize(n=n())%>%
#   filter(n>5)
# test$StationFront
# 
# acc4<-acc3%>%
#   filter(StationFront%in%test$StationFront)


test<-acc3%>%
  group_by(StationFront,Group,estabdate)%>%
  summarize(enddate=format(max(mdy(sampledate)), "%d/%m/%Y"))

acc4<-acc3%>%
  left_join(test)
data.frame(acc4)



##### Calculating one accretion rate per plot via regression ####

#Doing calcs on all plots
#after checking the data, I filtered plots that had fewer than 6 measurements, since a slope on n=5 is sketchy. and that is what Jankowski did
acc5<-acc4%>%
  group_by(StationFront,estabdate,enddate)%>%
  summarise(acc=as.numeric(coef(lm(meanaccmm~years))[2]),n=n(),lat=mean(lat),lon=mean(lon))%>%
  filter(n>5)
acc5<-as.data.frame(acc5)
acc5
#258 sites


#check that it is same as above, yes
acc5[acc5$StationFront=="CRMS0549",]
sort(acc5$acc)
acc5[which(acc5$acc<0),]
acc5[which(acc5$acc>100),]
#this note is only relevant from the early part of the time series I think (like the first 9 years or something): the plot CRMS0174 has a value over 100mm/yr, this looks actually correct based on the data, although there is a note in the raw data file that one measurement was affected by a large storm deposit, so I could delete if it looks really odd. there is no veg data from this station






