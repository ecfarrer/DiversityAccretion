
biomass <- read.csv("Data/CRMS_Biomass17Jan25.csv",stringsAsFactors = T)
head(biomass)



soil<-read.csv("Data/CRMS_Soil21Feb2025.csv",stringsAsFactors = T)

soil2<-soil%>%
  separate_wider_delim(Station.ID,delim = "-",names = c("StationFront",NA),cols_remove = F)%>%
  filter(StationFront%in%unique(dat$StationFront))%>%
  rename(SampleDate=Sample.Date..mm.dd.yyyy.,StationID=Station.ID,Depthcm=Sample.Depth..cm.,WetSoilpH=Wet.Soil.pH..pH.units.,Salinity=Soil.Salinity..ppt.,SoilMoisture=Soil.Moisture.Content....,BulkDensity=Bulk.Density..g.cm3.,OM=Organic.Matter....,BelowgroundLive=Belowground.Live.Biomass..g.m2.,BelowgroundDead=Belowground.Dead.Biomass..g.m2.,TotalCgkg=Total.Carbon..g.kg.,TotalNgkg=Total.Nitrogen..g.kg.,TotalPmgkg=Total.Phosphorus..mg.kg.,Sand=Sand....,Silt=Silt....,Clay=Clay....,ParticleSizeMean=Particle.Size.Mean..phi.,SoilLat=Latitude,SoilLon=Longitude)%>%
  select(StationFront,StationID,SampleDate,Depthcm,WetSoilpH,Salinity,SoilMoisture,BulkDensity,OM,BelowgroundLive,BelowgroundDead,TotalCgkg,TotalNgkg,TotalPmgkg,Sand,Silt,Clay,ParticleSizeMean,SoilLat,SoilLon)%>%
  group_by(StationFront,StationID,SampleDate)%>%
  relocate(BelowgroundLive:BelowgroundDead,.after=Depthcm)

#There are some plots with NA for dead but a value for live (but all dead rows have a value for live)
#which(soil2$BelowgroundLive>0&is.na(soil2$BelowgroundDead)==T)
#which(soil2$BelowgroundDead>0&is.na(soil2$BelowgroundLive)==T)

#There are lots of levels for Depthcm, spaces before numbers are an issue, but also biomass and ph etc data were on different depth splits. look more into this below.
levels(soil2$Depthcm)

soil3<-soil2%>%
  select(StationFront:BelowgroundDead,SoilLat,SoilLon)%>%
  filter(is.na(BelowgroundLive)==F)%>%
  mutate(Depthcm=droplevels(Depthcm))

#There are still all sorts of divisions in the depth. 0-13, 0-17. Generally it is usually 0-8, 8-16, 16-24 and sometimes 24-32. I could remove all the weird ones, sum everything over whatever depths are listed, or standardize to a certain depth. for now I will only use cores with three depths of 0-8, 8-16, 16-24
sort(levels(soil3$Depthcm))
head(data.frame(soil3))
View(soil3)
data.frame(soil3%>%
  group_by(Depthcm)%>%
  summarise(n=n()))

soil4<-soil3%>%
  filter(Depthcm%in%c("0 to 8","8 to 16","16 to 24"))

#most stationIDs were sampled just once ever. three plots CRMS0498 1,2,and3 were sampled twice a day apart.
temp<-soil4%>%
  group_by(StationID,SampleDate)%>%
  summarize(n=n())%>%
  filter(n==3)
data.frame(temp%>%
  group_by(StationID)%>%
  summarise(n2=n()))

soil5<-soil4%>%
  filter(StationID%in%unique(temp$StationID))%>%
  group_by(StationFront,StationID,SampleDate)%>%
  summarise_at(vars(BelowgroundLive:BelowgroundDead),sum,na.rm=T)%>%
  group_by(StationFront)%>%
  summarise_at(vars(BelowgroundLive:BelowgroundDead),mean,na.rm=T)

head(soil5)
