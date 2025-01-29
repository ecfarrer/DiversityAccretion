#Veg data

library(tidyverse)
library(ggplot2)
# library(plotrix)
# library(vegan)
# library(nlme)
# library(chron)
# library(vegan)

##### Raw veg data ##### 
#Braun-Blanquet Rank: 5 => 75 percent cover; 4 = 50-75 percent cover; 3 = 25-50 percent cover; 2 = 5-25 percent cover; 1 = numerous, but less than 5 percent cover, or scattered, with cover up to 5 percent; + = few, with small cover; and r = rare, solitary, with small cover.

#Note that %CoverTotal is not necessarily all the covers added together - I'm finding (and guessing/interpreting) that if there was a tree trunk in the plot, then it the trunk does not count as "total emergent veg cover" and that sometimes if it is like 40 herb and 40 carpet, the total cover is still 40% (if they overlap, it doesn't count double).

veg <- read.csv("Data/CRMS_Veg17Jan25.csv",stringsAsFactors = T)
head(veg)
unique(veg$Sample.Type)
unique(veg$Vegetation.Type)
unique(veg$In.Out)

#Change all Solitary and <1 to 0.5 cover. The species that have "solitary" are mostly grasses or herbaceous/seedling baldcypress/other woody plants
veg$X..Cover<-as.character(veg$X..Cover)
veg$X..Cover[which(veg$X..Cover=="Solitary")]<-0.5
veg$X..Cover[which(veg$X..Cover=="<1")]<-0.5
veg$X..Cover<-as.numeric(veg$X..Cover)

#Change all <1 in total cover to 0.5.
veg$X..Cover.Total<-as.character(veg$X..Cover.Total)
veg$X..Cover.Total[which(veg$X..Cover.Total=="<1")]<-0.5
veg$X..Cover.Total<-as.numeric(veg$X..Cover.Total)


#Cleaning with dplyr: 
#remove swamp (select only marshes)
#remove planted plots (not sure why there are a few of these listed, they are not part of the regular CRMS000 plots
#selection only In.Out is Both or In (Out means the plant was outside of the plot and it won't have a cover value)
#Replace the diamond character which was read in as "\xd7" for Agropogon withnothing
#Take out the rows were Species was "Water" (from 2006). This was a % cover of water, and it only happened when there are actually other plant species in the plot. As far as I can tell, there were never any plots in 2006 with only water, so I can delete that "water" row and that is fine.
#RTake out rows where the Species is "Bare Ground" (from 2006)
#Pawel used iconv and gsub to remove special characters in species names, but I don't see any special characters in the species names
#The plots where phrag was estimated due to the phrag stand being too large and the crew couldn't get to the plot are listed as NA with % cover in the comments, so they will be filtered out b/c there is no data (%covertotal is NA, and the species are all "out"). It would probably be fine to go in and put 100% phrag in those plots, but I won't for now
#There are some "both" that have NA for a cover, I saw it for plots when there was an alligator nest so it couldn't be surveyed, and when data was suspect so Quality Control deleted it, and with some trees where there was open water, not sure why there was no cover at all for that species. So I should filter NAs for Cover 

veg2<-veg%>%
  rename(StationID = Station.ID, CollectionDate = Collection.Date..mm.dd.yyyy.,
         Cover = X..Cover, TotalCover = X..Cover.Total,
         Species = Scientific.Name.As.Currently.Recognized)%>%
  filter(Community%in%c("Freshwater","Intermediate","Brackish","Saline"))%>%
  filter(Vegetation.Type=="Natural")%>%
  filter(In.Out%in%c("Both","In"))%>%
  select(StationID, CollectionDate, Community, TotalCover, Species, Cover,Comments,Latitude,Longitude)%>%
  mutate(across(Species, ~ str_remove_all(.x, '\xd7')))%>%
  filter(Species!="Water")%>%
  filter(Species!="Bare Ground")%>%
  filter(!is.na(Cover))
  
head(veg2)
sort(unique(veg2$Species))
sort(unique(veg2$TotalCover))
veg2[which(veg2$TotalCover==0),]

#if %CoverTotal is 0 it means the plot was open water, not sure if I can somehow keep those plots in, no species are listed so InOut is NA. Bareground and open water are sometimes given percent covers and sometimes are NA


#Add back in the plots that had no plants.

veg2zerocover<-veg%>%
  rename(StationID = Station.ID, CollectionDate = Collection.Date..mm.dd.yyyy.,
         Cover = X..Cover, TotalCover = X..Cover.Total,
         Species = Scientific.Name.As.Currently.Recognized)%>%
  filter(Community%in%c("Freshwater","Intermediate","Brackish","Saline"))%>%
  filter(Vegetation.Type=="Natural")%>%
  filter(TotalCover==0)%>%
  group_by(StationID,CollectionDate,Community,TotalCover,Comments,Latitude,Longitude)%>%
  tally()%>%
  mutate(Species="Water")%>%
  mutate(Cover=0.001)%>%
  select(StationID, CollectionDate, Community, TotalCover,Species,Cover,Comments,Latitude,Longitude)

#(done prior to group_by) The only plots with Cover have only species "Bare Ground" at 100% (there were some plots with other species like Acer and 0 TotalCover but I think they were swamps and I filtered those out)
#veg2zerocover[which(veg2zerocover$Cover>0),]

#(done prior to group_by) Are any plots/dates duplicated, yes, this is probably due to many species that are "out" for In/Out
#length(unique(paste(veg2zerocover$StationID,veg2zerocover$CollectionDate)))
#dim(veg2zerocover)

#1894 rows 
head(data.frame(veg2zerocover))
dim(veg2zerocover)
length(unique(paste(veg2zerocover$StationID,veg2zerocover$CollectionDate)))
unique(veg2zerocover$StationID) 
#452 plots

#Merge back these open water plots
veg3<-rbind(veg2,veg2zerocover)








levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"
veg$Community<-factor(veg$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))

length(unique(veg$StationID))
length(unique(veg$StationFront))

#not sure what this means: So I should filter NAs for Cover if I use the %CoverTotal==0






#Split the CollectionDate into 3 columns: day/month/year:
crms2<-crms1a %>% separate(col = CollectionDate ,
                           into=c("month","day","year"),sep="/",
                           remove = F)
#Select time and veg set you want to work with:======
crms3 <- filter(crms2,  year < 2018  &  year > 2006)


#Inspect the comments=====
i1 <- grepl("Phragmites\\scover\\swas\\sestimated\\sto\\sbe", crms3$Comments)# \\s = regular expression for space
sum(i1==TRUE)#964 plots with comment on estimated Phragmites cover as the sites were not surveyed
#These i1 sites were too dense to access.
#Phragmites was estimated between 70% & 100%
crms3$Category <- i1 
crms3$Category2 <- ifelse(crms3$Category==TRUE, "PhragInComment", "NA") #extra column to keep track of what TRUE means
table(crms3$In.Out[crms3$Category=="TRUE"])#All Phragmites-in-comment was recorded as OUT:
#Both   In  Out 
#   0    0  964 
#crms3$In.Out[crms3$Category== TRUE] <- "Both" #change In.Out to Both to keep this record.

#substitute - for _ so:
crms3$StationID<-as.factor(sub("-","_",crms3$StationID))

#Keep "Out" out: Column In.Out assigns IN/OUT/BOTH categories to all species.
#If a species occurs both IN and OUT of a plot, it is recorded as BOTH. 
#If a species is rooted OUT of the plot, but is hanging over the plot, it is also recorded as BOTH.
#Cover measures the cover within the quadrat only. Out-records add to CoverTotal value.

crms4 <- filter(crms3, In.Out != "Out")
dim(crms4) # 191405     13

#Rename species to standard specCode (vegan-friendly):=====
#seperate complex spName into genus and species:
crms5 <- crms4  %>% separate(col = SpName, into = c("genus", "species", "extra_sp_info"),
                             fill= "right", extra= "merge", sep= "\\s" , remove = F)

#Cut 4 letters of genus and 11 of species and turn into specCode (Vegan-friendly):
crms6 <- crms5 %>% mutate(spec = strtrim(genus, 4), Code = strtrim(species, 11)) %>%
  unite(specCode, spec, Code, sep = "_") %>%
  select( StationID, day, month, year, Community, Cover, Category2,SpName, genus, species, specCode) %>%
  mutate (StationID.year = interaction(StationID,year),
          genus.species  = interaction(genus,species)) #extra column
dim(crms6)# 191405     13
length(unique(crms6$specCode))#570
unique(crms6$specCode)

#Remove duplicates if any:
crms7<- distinct(crms6)
crms7$i <- 1:nrow(crms7)#assign id to each row to make "spread" function work smoothly.
dim(crms7)#  191405     14

#Remove Swamp Comunities 
#Mutate Cover (<1 and Solitary) to numeric value:
#Replace "<1" with 0.5 and "Solitary" with 0.1 (CPRA-recommended)

crms8  <-   filter(crms7, Community !="Swamp") %>%
  mutate(Cover = recode(Cover, "<1" = "0.5")) %>%
  mutate(Cover = recode(Cover, "Solitary" = "0.1"))

sum(is.na(crms8$Cover))#Double check if NA-s produced = 0, If NA-s turn them to 0= is.na(crms8$Cover) <-0
dim(crms8)# 159727     14

#Turn factors into numeric values and characters into factors:=====
crms8$Cover <- as.numeric(as.character(crms8$Cover))
crms8$Community <- factor(crms8$Community)
crms8$specCode <- factor(crms8$specCode)
crms8$StationID <- factor(crms8$StationID)
crms8$year<- factor(crms8$year)
crms8$specCode<- factor(crms8$specCode)
str(crms8)

length(unique(crms8$specCode))#455
(unique(crms8$specCode))#Check out new standarized R-friendly plant species names

#Spread specCode into columns (produce final Veg matrix):=====
crms8$specCode <- factor(crms8$specCode)#to remove empty levels.

#Check if specCode-s duplicate (the same specCode assigned to different species)====
#It should be 455 rows of specCodes:
sp <-  select(crms8, - c(genus, species, genus.species, Category2))%>%
  group_by(specCode, SpName) %>%
  summarise(n=n()) %>% distinct()
str(sp) # sp$ specCode     : Factor w/ 455 levels
#Check if any specCode is duplicated:
spCode <- as.data.frame(sp$specCode)
specCode_duplicated <- spCode[duplicated(spCode),]
specCode_duplicated #Ludw_grandiflora and  Phra_australis but these look OK!
#Ludwigia grandiflora Michx Greuter  Burdet versus Ludwigia grandiflora Michx Greuter  Burdet ssp hexapetala Hook  Arn GL Nesom  Kartesz
#Phragmites australis Cav Trin ex Steud versus Phragmites australis Cav Trin ex Steud ssp australis

#Checking if these two species are recorded togehter in one plot (observation unit):
Ludw_grandiflora <- crms8 %>% filter (specCode=="Ludw_grandiflora")%>%
  select (StationID, specCode, SpName, year) %>% group_by(StationID, SpName, year) %>% summarise(n=n())
Ludw_grandiflora
range(Ludw_grandiflora$n)#1 1 = means no two SpName-s at the same plot, good to spread()

Phra_australis <- crms8 %>% filter (specCode=="Phra_australis")%>%
  select (StationID, specCode, SpName, year) %>% group_by(StationID, SpName, year) %>% summarise(n=n())
Phra_australis
range(Phra_australis$n)#1 1 = means no two SpName-s at the same plot, good to spread()

#Save crms8 for joining species names with trait data:
#write.csv(sp, file = "CRMS_Marsh_Veg_SpeciesList.csv" )


#As per email from CPRA in regards to duplicated records (LeighAnne.Sharp@la.gov):
#That would be the regularly scheduled vegetation assessment. 
#The second trip in October was conducted by CPRA (then DNR) staff.
#That would be a post hurricane assessment (H. Gustav was 9/1/08; H. Ike was 9/13/08). 
#We re-sampled select sites after the 2008 storms.  You can sort those out by Organization.  
#The data is spread across post- and prio-hurricance months (06-10) 
sort( table (crms8$month))
#12    11    05    06     6     7    10    09     9     8    07    08 
#24    28   402   875  1018  4829  7016  7927 13006 38459 41789 44354

#Find duplicate StaionFront.month records and remove the latter ones in duplicated records:
#Fix the month names first:
crms8$month <- as.factor(crms8$month)
levels(crms8$month)[levels(crms8$month)== "05"] <- "5"
levels(crms8$month)[levels(crms8$month)== "06"] <- "6"
levels(crms8$month)[levels(crms8$month)== "07"] <- "7"
levels(crms8$month)[levels(crms8$month)== "08"] <- "8"
levels(crms8$month)[levels(crms8$month)== "09"] <- "9"
sort( table (crms8$month))
#12    11     5     6    10     9     7     8 
#24    28   402  1893  7016 20933 46618 82813

#Create site (StationFront) variable:
crms8.dupl <- separate(crms8, StationID, into = c("StationFront", "StationBack"), 
                       sep = "_", remove = FALSE)
#Create Station.Front.month.year variable:
crms8.dupl$StationID.year<- interaction(crms8.dupl$StationID,
                                        crms8.dupl$year)

#See if any StationFront was measured more than once (in two different montns):
Find.duplicated <- crms8.dupl %>%
  group_by(StationID.year, month) %>%
  summarise(TotalCover = sum(Cover), N = n()) 
dim(Find.duplicated)# 35232     4
#View(Find.duplicated) 
#Create a  data.frame with duplicated surveys:
duplicated.ID <- Find.duplicated[duplicated(Find.duplicated$StationID.year),]
as.data.frame(duplicated.ID) #25

#Create StationID.year.month variable to ID rows to remove
duplicated.ID$StationID.year.month <- interaction (duplicated.ID$StationID.year,
                                                   duplicated.ID$month)
#Create StationID.year.month variable  in crms8 dataset to match records in duplicated.ID
crms8.dupl$StationID.year.month<- interaction(crms8.dupl$StationID.year,
                                              crms8.dupl$month)


#Remove the duplicated levels of StationID.year.month:
crms8c <- crms8.dupl [ ! crms8.dupl$StationID.year.month %in% duplicated.ID$StationID.year.month, ]
dim(crms8c)#159677     17
dim(crms8.dupl)#159727     17

#Other good way to fix duplicated surveys issue is to run average of Covers on specific group:
#crms8_clean <- crms8c %>%  group_by(StationID.year, Community, specCode, year) %>%  summarise(Cover = mean(Cover))
#But we agreed with Emily & Christina that removing the duplicated records is more sound.

#Create wide species matrix:
crms8_clean <- select(crms8c,  StationID.year, Community, specCode, year, Cover)
crms9 <- spread (crms8_clean, key = specCode, value = Cover, fill = 0) #WORKS NOW after removing duplicated surveys!
dim(crms9)#159677    470

#Split StationID (site.plot) into StationFront(site) and StationBack (plot 2mx2m)
crms9 <- separate(crms9, StationID.year, into = c("StationFront", "StationBack"), sep = "_", remove = FALSE)
dim(crms9) #35207   460

#DOUBLE-CHECK DATA with Excel Raw File=========
crms9$StationFront.year<-interaction(crms9$StationFront, crms9$year)#We need that for joining
x <- select(crms9, StationFront.year, Spar_patens) %>%
  filter(StationFront.year == "CRMS0002.2008")
mean(x$Spar_patens)#All good if =  66.7%, we got this value in pivot table in raw csv file.

#Remove NA, WateNA (open water) and BareGround:====
delete <- c("_NA", "BareGround", "WateNA")
v1 <- crms9[, !(names(crms9) %in% delete)] 
dim(v1)# 35207   460

#check if all rows > 0:
vegveg <- v1[ , 6:458]
occur.row<-apply(vegveg,1,sum)
zeroRows<-vegveg[occur.row <= 0 ,]
nrow(zeroRows)# 250 zero rows to be removed

#check if all columns > 0:
occur.col<-apply(vegveg,2,sum)
oneColumns<-vegveg [,occur.col < 0]
ncol(oneColumns)#0

#Remove zero rows from veg data:
v2 <- v1[ ! occur.row == 0 ,]
dim(v2)# 34957   460
names(v2)
v3 <- separate(v2, StationID.year, into = c("StationID", "RemoveThisYear"),
               sep = "\\.", remove = FALSE)
dim(v3)#34957   462
v4 <- select (v3, -RemoveThisYear)
dim(v4)#34957   461

z <- select(v4, StationFront.year, Spar_patens) %>%
  filter(StationFront.year == "CRMS0002.2008")
mean(z$Spar_patens)#All good if mean(z) =  66.7%, we got this value in pivot table in raw csv file.

#Create new clean "CRMS_Marsh_Veg.csv":
#write.csv(v4, file = "CRMS_Marsh_Veg.csv", row.names = FALSE)
#Proceed to: 02_CLEAN_CRMS_Hydro, 03_CLEAN_CRMS_Soil, 04_MERGE_VegHydroSoil 

#Could filter out plots sampled in month 10 or 11 or 12 if I wanted to try harder to make sure that post-hurricanes were not sampled. or I could only look at plots sampled in 10,11,12 if I wanted to make sure only post hurricane is sampled, however there aren't many plots like this
#Make data wide
veg2<-veg%>%
  select(StationID,StationFront,StationBack,month,day,year,Community,CoverTotal,Cover,SpecCode)%>%
  spread(SpecCode,Cover,fill=0)
veg2[1:15,1:15]
colnames(veg2)
max(veg2$year)

#Make sure each plot has a community for every year (or at least 6 of the 10 years) 2007-2016
table(veg2$year)

counts<-veg2%>%
  group_by(StationID)%>%
  summarise(n())
counts2<-counts$StationID[which(counts$`n()`>5)]
veg3<-veg2[which(veg2$StationID%in%counts2),]
length(unique(veg3$StationID))
table(veg3$year)
#there must be some duplicate years

#take out the october measurement from years that are duplicates
counts<-veg3%>%
  group_by(StationID,year)%>%
  summarise(n())
counts[which(counts$`n()`!=1),]

veg3<-veg3[-which(veg3$StationID=="CRMS0121_V34"&veg3$month==10),]#
veg3<-veg3[-which(veg3$StationID=="CRMS3601_V27"&veg3$month==10),]
veg3<-veg3[-which(veg3$StationID=="CRMS3601_V57"&veg3$month==10),]
table(veg3$year)

veg3[1:15,1:15]


# make a commtype2007 vector
# instead of doing this, I switched to use Pawel's method of taking the community type that was most common
# Commtype2007<-veg3 %>%
#   filter(year==2007)%>%
#   select(StationID,Community)
# 
# length(unique(veg3$StationID))-dim(Commtype2007)[1]
# #342 stations were not sampled in 2007
# notsampled2007<-setdiff(veg3$StationID,Commtype2007$StationID)
# 
# Commtype2008<-veg3 %>%
#   filter(year==2008,StationID %in% as.factor(notsampled2007))%>%
#   select(StationID,Community)
# dim(Commtype2008)
# #225 sampled in 2008, 117 left
# 
# notsampled20072008<-setdiff(notsampled2007,Commtype2008$StationID)
# length(notsampled20072008)
# 
# Commtype2009<-veg3 %>%
#   filter(year==2009,StationID %in% as.factor(notsampled20072008))%>%
#   select(StationID,Community)
# dim(Commtype2009)
# #77 more, 40 left
# 
# notsampled200720082009<-setdiff(notsampled20072008,Commtype2009$StationID)
# length(notsampled200720082009)
# 
# Commtype2010<-veg3 %>%
#   filter(year==2010,StationID %in% as.factor(notsampled200720082009))%>%
#   select(StationID,Community)
# dim(Commtype2010)
# #31, 9 left
# 
# notsampled2007200820092010<-setdiff(notsampled200720082009,Commtype2010$StationID)
# length(notsampled2007200820092010)
# 
# Commtype2011<-veg3 %>%
#   filter(year==2011,StationID %in% as.factor(notsampled2007200820092010))%>%
#   select(StationID,Community)
# dim(Commtype2011)
# 
# Commtype2007b<-rbind(Commtype2007,Commtype2008,Commtype2009,Commtype2010,Commtype2011);colnames(Commtype2007b)[2]<-"Community2007"
# 
#this is where I merged them before
#veg4<-merge(veg3,Commtype2007b)


#Using Pawel's method:
#first define community type by most common type in stationfront
#then average species comp and div and species rich
#then merge accretion data with plant community data (veg4)

#Define the community-type in each StationFront based on n of Commuity-type Counts per station:
StationComm <- group_by(veg3,StationFront,Community) %>% 
  count(Count=StationFront)
StationComm#It gives us count of communities per StationFront (740)
SCwide<- spread(StationComm, key = Community, value = n, fill = 0)#make it wide
SCwide$WhichMax<-colnames(SCwide)[apply(SCwide,1,which.max)]#while wide we can see which Comm is predominant
SCwide
StationCommDefined<-SCwide[, c(1,7)]
colnames(StationCommDefined)[2] <- "Community" #Renaming WhichMAx back to Community
StationCommDefined #320 stationFronts


#Join with veg3 to create a plot/year-level (StationID*year level) data set
veg4<-veg3%>%
  rename(Community.yr=Community)%>%
  left_join(StationCommDefined,by="StationFront")
  
veg4$richness<-specnumber(veg4[,9:438])
veg4$shannon<-diversity(veg4[,9:438],index="shannon")
veg4$tot<-rowSums(veg4[,9:438])

#rearrange columns
veg5<-veg4%>%
  select(StationID:Community.yr,Community,CoverTotal,richness:tot,Acerrubr:ZiziMill)


#Then create a StationFront*year-level dataset
#Summarize means across species/diversity by stationfront and year. there will be an NA for meanaccmmpery when there is veg data but no acc data
veg6<-veg5%>%
  group_by(StationFront,Community,year)%>%
  summarise_at(vars(CoverTotal:ZiziMill),mean,na.rm=T)

#then replace richness, shannon, and tot, if you want to recalculat them based on the new averaged stationfront-level species data (rather than have them be the average across the small plots)
veg6$richness<-specnumber(veg6[,8:437])
veg6$shannon<-diversity(veg6[,8:437],index="shannon")
veg6$tot<-rowSums(veg6[,8:437])


#Then create a StationFront-level dataset (average over years)
veg7<-veg6%>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(CoverTotal:ZiziMill),mean,na.rm=T)



#veg only questions
#Does phrag increase over time?

m1<-veg5%>%
  group_by(year,Community)%>%
  summarise(mean=mean(Phraaust),se=std.error(Phraaust))
ggplot(m1,aes(x=year,y=mean,color=Community))+
  labs(x = "",y="",colour="Community type")+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,scale="free")



