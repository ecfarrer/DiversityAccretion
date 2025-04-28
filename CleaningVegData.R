#Veg data

##### Raw veg data ##### 
#Braun-Blanquet Rank: 5 => 75 percent cover; 4 = 50-75 percent cover; 3 = 25-50 percent cover; 2 = 5-25 percent cover; 1 = numerous, but less than 5 percent cover, or scattered, with cover up to 5 percent; + = few, with small cover; and r = rare, solitary, with small cover.

#Note that %CoverTotal is not necessarily all the covers added together - I'm finding (and guessing/interpreting) that if there was a tree trunk in the plot, then the trunk does not count as "total emergent veg cover". also the cover can be from tree leaves that are rooted outside the plot but are covering the plot so the totalcover=0 and the tree species still have "cover". also sometimes if it is like 40 herb and 40 carpet, the total cover is still 40% (if they overlap, it doesn't count double). 

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
#The levels of Community are NA, Brackish, Freshwater, Intermediate, Saline, Swamp. I will keep all of them. The NA are typically plots that either werent sampled due to dought and the boat couldn't get there OR open water (but not all open water plots have NA, some have what the community of the out vegetation is)
#remove planted plots (not sure why there are a few of these listed, they are not part of the regular CRMS000 plots
#selection only In.Out is Both or In (Out means the plant was outside of the plot and it won't have a cover value. this also takes out NAs)
#The plots where phrag was estimated due to the phrag stand being too large and the crew couldn't get to the plot are listed as Cover=NA with phrag % cover written in the comments%covertotal is NA, and the species are all "out", so they will be filtered out b/c the species are all out. I will investigate more below
#Replace the diamond character which was read in as "\xd7" for Agropogon withnothing
#Take out the rows were Species was "Water" (from 2006). This was a % cover of water, and it only happened when there are actually other plant species in the plot. As far as I can tell, there were never any plots in 2006 with only water, so I can delete that "water" row and that is fine.
#Take out rows where the Species is "Bare Ground" (from 2006)
#Pawel used iconv and gsub to remove special characters in species names, but I don't see any special characters in the species names
#There are some "both" that have NA for a cover, I saw it for plots when there was an alligator nest so it couldn't be surveyed, and when data was suspect so Quality Control deleted it, and with some trees where there was open water, not sure why there was no cover at all for that species, and for swamps there are a few plots that look like they forgot to record cover for baldcypress and maple b/c in the comments the canopy cover was like 80% but there were no covers listed for those species (even though they were called "both") but I might be misunderstanding "canopy cover". Related to when I was investigating the last point, there were also some swamp plots that had been mislabeled in some years (as mentioned in comments) and I don't think this was corrected. There are some plots int he comments that say a tagged tree is in the plot but the baldcypress is not listed in the species at all (maybe it is just a bit of the trunk and it is leaning out?). I will filter NAs for Cover b/c there is nothing else I can do

veg2<-veg%>%
  rename(StationID = Station.ID, CollectionDate = Collection.Date..mm.dd.yyyy.,
         Cover = X..Cover, TotalCover = X..Cover.Total,
         Species = Scientific.Name.As.Currently.Recognized)%>%
  mutate(StationIDCollectionDate=paste(StationID,CollectionDate,sep=""))%>%
  filter(Vegetation.Type=="Natural")%>%
  filter(In.Out%in%c("Both","In"))%>%
  mutate(across(Species, ~ str_remove_all(.x, '\xd7')))%>%
  filter(Species!="Water")%>%
  filter(Species!="Bare Ground")%>%
  filter(!is.na(Cover))%>%
  select(StationID, CollectionDate, StationIDCollectionDate, Community, TotalCover, Species, Cover,Comments,Latitude,Longitude)
  
head(veg2)
#View(veg2[which(is.na(veg2$Cover)),])
#View(veg2[which(veg2$TotalCover==0),])

#Some swamp plots have 0 TotalCover but a number for Cover (because tree leaves overlapping the plot without the plant being rooted?)
#This is the StationIDCollectionDate for those 33 plot-times. 89 rows were removed from the veg2zerocover this looks correct because many plots had many species listed as Out so cover is NA (so it is not just one line per stationIDcollectiondate)
indzeronumber<-veg2$StationIDCollectionDate[which(veg2$TotalCover==0)]


##### Extract and add back in the plots that had no plants #####
#if %CoverTotal is 0 it means the plot was open water or bareground (for marsh plots is usually water, for swamp it was usually bareground), I'm going to add them back in, no species are listed so InOut is NA. Bareground and open water are sometimes given percent covers and sometimes are NA. 

veg2zerocover<-veg%>%
  rename(StationID = Station.ID, CollectionDate = Collection.Date..mm.dd.yyyy.,
         Cover = X..Cover, TotalCover = X..Cover.Total,
         Species = Scientific.Name.As.Currently.Recognized)%>%
  mutate(StationIDCollectionDate=paste(StationID,CollectionDate,sep=""))%>%
  filter(Vegetation.Type=="Natural")%>%
  filter(TotalCover==0)%>%
  filter(!StationIDCollectionDate%in%indzeronumber)%>%
  group_by(StationID,CollectionDate,StationIDCollectionDate,Community,TotalCover,Comments,Latitude,Longitude)%>%
  tally()%>%
  mutate(Species="Water BareGround")%>%
  mutate(Cover=0.001)%>%
  select(StationID, CollectionDate, StationIDCollectionDate, Community, TotalCover,Species,Cover,Comments,Latitude,Longitude)

#(done prior to group_by) The only plots with Cover have only species "Bare Ground" at 100%
# veg2zerocover[which(veg2zerocover$Cover>0),]
# View(veg2zerocover[which(veg2zerocover$Cover>0),])
# veg2zerocover[which(veg2zerocover$Community=="Swamp"),]

#(done prior to group_by) Are any plots/dates duplicated, yes, this is probably due to many species that are "out" for In/Out
#length(unique(paste(veg2zerocover$StationID,veg2zerocover$CollectionDate)))
#dim(veg2zerocover)

#2282 plot-times (1894 rows when not including swamps)
head(data.frame(veg2zerocover))
dim(veg2zerocover)
length(unique(paste(veg2zerocover$StationID,veg2zerocover$CollectionDate)))
unique(veg2zerocover$StationID) 
#556 plots (452 plots not including swamps), many plots were repeatedly open water (not surprising)


#Merge back these open water plots
veg3<-rbind(veg2,veg2zerocover)%>%
  arrange(StationID, as_date(mdy(CollectionDate)))
head(veg3)

#Check a few plots, it seems to have worked
#View(veg3[which(veg3$StationID=="CRMS2627-V56"),])
#Check that the length of all the waterbareground lines 2282 is equal to those extracted stationIDcollectiondates (i.e. check that those plots that we added back were not already in the veg dataset with a numeric cover, i.e. check wether no plot has waterbareground and any other species)
# ind<-veg3$StationIDCollectionDate[which(veg3$Species=="Water BareGround")]
# length(ind)
# dim(veg3[which(veg3$StationIDCollectionDate%in%ind),])




##### Look into plots that were not accessible due to too much phrag ##### 

#There are estimates of phrag cover but it is at the edge of the stand, not in the plot. In the comments there is only an estimate of phrag cover, not any of the other species cover. Also, sometimes there are LOTS of other species listed so who knows if they would be in the actual plot (including all of them would be an overestimate of diversity and we can't include them anyway b/c there is no cover estimate.) So I think I probably have to just delete them.

sum(grepl("Phragmites\\scover\\swas\\sestimated\\sto\\sbe", veg$Comments))# \\s = regular expression for space #902 rows, but the same comment is written in the same plot-year when there are many species "out". 
ind<-grepl("Phragmites\\scover\\swas\\sestimated\\sto\\sbe", veg$Comments)
temp<-veg[ind,]
dim(temp)
length(unique(temp$Station.ID)) #165 stations
length(unique(paste(temp$Station.ID,temp$Collection.Date..mm.dd.yyyy.))) #604 station-years
#165 stations were affected. There are 604 station-years so that means many stations had this problem 2-4 years in a row. the 902-604=298 is the other "out" species listed





##### Clean up Veg3, make species codes #####

#make the blank factor levels into NA and do not use NA as a factor level (this is best practice)
which(veg3$Community=="")
veg3$Community<-factor(veg3$Community, levels = c("Swamp", "Freshwater","Intermediate","Brackish","Saline"))
which(is.na(veg3$Community))

#There are two ssp no var
#There is a Ludwigia grandiflora subspecies that would get lumped if I used the code below. there are not many of the subspecies and it tends to pop up and then go away so I can't tell if the IDing was consistent over time. They are not in the same plot, so I can just call the subspecies "Ludwigia grandiflora"
#There is Phragmites australis (Cav.) Trin. ex Steud. ssp. australis in only 2 plots, i don't think this was consistently IDed to subspecies every year, so I will sum phrag and phrag ssp australis. However they are not in the same plot (see below), so I can just call them both Phragmites australis (Cav.) Trin. ex Steud. 

veg3[which(veg3$Species=="Ludwigia grandiflora (Michx.) Greuter & Burdet ssp. hexapetala (Hook. & Arn.) G.L. Nesom & Kartesz"),]
veg3[which(veg3$Species=="Ludwigia grandiflora (Michx.) Greuter & Burdet"),]

temp<-sort(unique(veg3$Species))
grep("ssp.",temp)
grep("var.",temp)

#There are 9 unknowns, "unknown" through "unknown #9" (there is no unknown#8), they are not consistently numbered across years so I should lump all unknowns as "unknown", but I will do it at the pivot_wider stage below, see note below
# sort(unique(veg3$Species))
# temp<-veg3[which(veg3$Species=="Unknown"),]
# temp1<-veg3[which(veg3$Species=="Unknown #1"),]
# temp2<-veg3[which(veg3$Species=="Unknown #2"),]
# temp3<-veg3[which(veg3$Species=="Unknown #3"),]
# temp4<-veg3[which(veg3$Species=="Unknown #4"),]
# temp5<-veg3[which(veg3$Species=="Unknown #5"),]
# temp6<-veg3[which(veg3$Species=="Unknown #6"),]
# temp7<-veg3[which(veg3$Species=="Unknown #7"),]
# temp9<-veg3[which(veg3$Species=="Unknown #9"),]

#there is at least one plots where there are two different unknowns
# intersect(temp1$StationIDCollectionDate,temp2$StationIDCollectionDate)#11
# intersect(temp1$StationIDCollectionDate,temp7$StationIDCollectionDate)#1
# intersect(temp$StationIDCollectionDate,temp1$StationIDCollectionDate)#1
# intersect(temp$StationIDCollectionDate,temp2$StationIDCollectionDate)#1

#NOT USED
# veg3b<-veg3%>%
#   mutate(Species=case_match(Species,"Unknown #1"~"Unknown","Unknown #2"~"Unknown","Unknown #3"~"Unknown","Unknown #4"~"Unknown","Unknown #5"~"Unknown","Unknown #6"~"Unknown","Unknown #7"~"Unknown","Unknown #9"~"Unknown",.default=Species))%>%
#   group_by(StationID,CollectionDate,StationIDCollectionDate,Community,TotalCover,Species,Comments,Latitude,Longitude)%>%
#   summarise(Cover=sum(Cover))

#sort(unique(veg3b$Species)) 
#dim(veg3b) #
#nrows before 335649 and after 335624 the summarize, so 25 were averaged over. There are a number of times that multiple unknowns were in the plot. I think what I will do is keep them as is until I pivot_wider, because for diversity/richness estimates I do want to know when different unknowns are in the same plot. But if I want to do ordinations, I should then average over all of them

#Separate into genus and species and other stuff
#Cut 5 letters of genus and 11 of species and make SpecCode
veg4<-veg3%>%
  separate(col = Species, into = c("genus", "species", "extra_sp_info"),
           fill= "right", extra= "merge", sep= "\\s" , remove = F)%>%
  mutate(Spec = strtrim(genus, 5), Code = strtrim(species, 11)) %>%
  unite(SpecCode, Spec, Code, sep = "_")%>%
  select(-extra_sp_info)%>%
  mutate(Year=year(mdy(CollectionDate)),Month=month(mdy(CollectionDate)))%>%
  unite(StationIDYear,c("StationID","Year"),remove=F,sep="_")
  
head(data.frame(veg4))

length(sort(unique(veg4$Species))) #670
length(sort(unique(veg4$SpecCode))) #668, correct, should be 2 shorter
dim(veg4)

str(veg4)
veg4$StationIDYear<-factor(veg4$StationIDYear)
veg4$StationIDCollectionDate<-factor(veg4$StationIDCollectionDate)
veg4$Species<-factor(veg4$Species)
veg4$genus<-factor(veg4$genus)
veg4$species<-factor(veg4$species)
veg4$SpecCode<-factor(veg4$SpecCode)

#Check that the phrag/phrag ssp and the ludwigia/ludwigia ssp are never in the same plot. this was done after the select(-extra_sp_info) line of code in the above chunk. Yes checked all good.

# temp1<-veg4[which(veg4$Species=="Ludwigia grandiflora (Michx.) Greuter & Burdet ssp. hexapetala (Hook. & Arn.) G.L. Nesom & Kartesz"),"StationIDCollectionDate"]
# temp2<-veg4[which(veg4$Species=="Ludwigia grandiflora (Michx.) Greuter & Burdet"),"StationIDCollectionDate"]
# intersect(temp1,temp2)
# 
# temp1<-veg4[which(veg4$Species=="Phragmites australis (Cav.) Trin. ex Steud."),"StationIDCollectionDate"]
# temp2<-veg4[which(veg4$Species=="Phragmites australis (Cav.) Trin. ex Steud. ssp. australis"),"StationIDCollectionDate"]
# intersect(temp1,temp2)




##### Remove duplicated plots, when pots were sampled twice in a year #####

#Remove duplicated efforts when plots were sampled two times in a year, possibly due to sampling pre- and post- hurricanes. Pawel's notes (from emailing LeighAnne.Sharp@la.gov) say that some were post hurricane, H. Gustav was 9/1/08, H. Ike was 9/13/08. the trips in october in 2008 were post hurricane. can also sort by organization
#for 2010, CRMS0097-F09VC was sampled in 7/23 and 8/6 2010, all the other subplots at CRMS0097 were sampled only 8/6/2010, so I should probably delete the july data from CRMS0097-F09VC
#for 2019, CRMS0319-VB03 is a strange plot also CRMS0319-VB01 and CRMS0319-VB02. these plots were only surveyed in 2019, CRMS0319-VB03 twice in 2019 on 6/11 and 6/26. I should eventually just delete all of these since they are never re-sampled in any other years. There are many other plots like this with VB in the stationback that should also be deleted. Thus I need a larger like of code to delete all these plots that were sampled only once. So for now, I will deal with the duplicate sampling date, the 6/11 comment is odd with mostly typha and a comment saying the marsh is floating, the 6/26 data looks more comparable to other plots at the site, so I will delete the 6/11 first sampling data
#for 2022, all 10 plots in CRMS4551-V03 were sampled on 6/15/22 and again on 6/28/22. however it looks like in 23 they sampled on 6/15/23 and the numbers are exactly the same from the numbers on 6/15/22 so I think it got duplicated somehow. So I will delete the first sampling on 6/15/22.

#for the 2008s, take the earlier date, delete the second sampling
#for 2020, all 10 plots in CRMS0209 were surveyed 6/11/20 and 7/15/20. this is odd. it looks like the data from 7/15/20 are a duplicate of next years data from 7/15/21. the numbers are the same and the month/day are the same too. so I should delete all the ones from 7/15 the second sampling
#for 2021, 7 plots from CRMS0395 were sampled in july and then resampled in october after hurricane Ida. v78,v79,v81 were only sampled post Ida, from the comments there was a thunder storm coming and they had to leave, some of the 7 sampled plots were not finished (although they looked pretty fine/had a lot of species). I will keep the pre-Ida for the ones I can. This is a little odd and I'm not sure the best way to do it. But now delete the second sampling.

temp2<-veg4%>%
  group_by(StationID,StationIDCollectionDate,StationIDYear,Year)%>%
  summarise(n=n())%>%
  group_by(StationID, StationIDYear, Year)%>%
  summarise(n=n())%>%
  filter(n>1)%>%
  arrange(Year,StationID)
data.frame(temp2)

veg5<-veg4%>%
  filter(StationIDCollectionDate!="CRMS0097-F09VC07/23/2010")%>%
  filter(StationIDCollectionDate!="CRMS0319-VB0306/11/2019")%>%
  filter(StationIDCollectionDate!="CRMS4551-V0306/15/2022")%>%
  filter(StationIDCollectionDate!="CRMS4551-V1806/15/2022")%>%
  filter(StationIDCollectionDate!="CRMS4551-V3006/15/2022")%>%
  filter(StationIDCollectionDate!="CRMS4551-V3306/15/2022")%>%
  filter(StationIDCollectionDate!="CRMS4551-V3506/15/2022")%>%
  filter(StationIDCollectionDate!="CRMS4551-V6006/15/2022")%>%
  filter(StationIDCollectionDate!="CRMS4551-V6306/15/2022")%>%
  filter(StationIDCollectionDate!="CRMS4551-V7006/15/2022")%>%
  filter(StationIDCollectionDate!="CRMS4551-V8106/15/2022")%>%
  filter(StationIDCollectionDate!="CRMS4551-V8206/15/2022")

temp2<-veg5%>%
  group_by(StationID,StationIDCollectionDate,StationIDYear,Year)%>%
  summarise(n=n())%>%
  group_by(StationID, StationIDYear, Year)%>%
  summarise(n=n())%>%
  filter(n>1)%>%
  arrange(Year,StationID)
data.frame(temp2)

#there are 43 plots for which I need to delete the second survey, unique temp1$StationIDCollectionDate should be 86 lines
temp1<-veg5[which(veg5$StationIDYear%in%temp2$StationIDYear),]
# temp1a<-temp1%>%
#   arrange(StationID,StationIDCollectionDate)
# identical(temp1$StationIDCollectionDate,temp1a$StationIDCollectionDate)
indtodelete<-unique(temp1$StationIDCollectionDate)[c(F,T)]

veg6<-veg5%>%
  filter(!StationIDCollectionDate%in%indtodelete)%>%
  arrange(SpecCode)
  #mutate(SpecCode=factor(SpecCode,levels=sort(unique(veg5$SpecCode))))
  

##### Output for Lecture 6 and homework 6 #####
#done before the arrange()
outlec6<-veg6%>%
  filter(Community=="Saline")%>%
  unite(GenusSpecies,genus,species,remove=F)%>%
  filter(GenusSpecies%in%c("Spartina_alterniflora","Juncus_roemerianus","Distichlis_spicata","Symphyotrichum_tenuifolium","Lythrum_lineare","Ipomoea_sagittata"))%>%
  select(StationID,Year,GenusSpecies,Cover)%>%
  filter(StationID!="CRMS0002-V54")%>%
  arrange(StationID,Year)

write.csv(outlec6,"/Users/farrer/Dropbox/EmilyComputerBackup/Documents/Teaching/EcologicalAnalysis2/EA2025/labs/lab6ggplot/crmssaline.csv")

outhom6<-veg6%>%
  unite(GenusSpecies,genus,species,remove=F)%>%
  filter(Community%in%c("Freshwater","Intermediate","Brackish","Saline"))%>%
  filter(GenusSpecies%in%c("Juncus_roemerianus","Panicum_hemitomon","Sagittaria_lancifolia","Schoenoplectus_americanus","Spartina_alterniflora","Spartina_patens"))%>%
  select(StationID,Year,Community,GenusSpecies,Cover)%>%
  filter(StationID!="CRMS0002-V54")%>%
  arrange(StationID,Year)%>%
  pivot_wider(names_from = GenusSpecies,values_from = Cover,values_fill = 0)%>%
  select(StationID, Year, Community, Juncus_roemerianus,Panicum_hemitomon,Sagittaria_lancifolia,Schoenoplectus_americanus,Spartina_alterniflora,Spartina_patens)
head(outhom6)
  
#add some NAs
outhom6[5002,5]<-NA
outhom6[5006,6]<-NA
outhom6[5017,4]<-NA
outhom6[5022,6]<-NA
which(outhom6$Panicum_hemitomon>0)
data.frame(outhom6[1:22,])
outhom6a<-outhom6[5000:7000,]
write.csv(outhom6a,"/Users/farrer/Dropbox/EmilyComputerBackup/Documents/Teaching/EcologicalAnalysis2/EA2025/labs/lab6ggplot/crmshmwk.csv",row.names = F)



##### Pivot wider and more cleaning #####

# Make a StationFront column
# Pivot wider
# Delete water/bareground
# Calculate summed cover
# Calculate richness, shannon, simpson, invsimpson
# Relocate those variables

veg7<-veg6%>%
  separate_wider_delim(StationID,delim = "-",names = c("StationFront",NA),cols_remove = F)%>%
  select(StationID,StationFront, Year,StationIDYear,CollectionDate,StationIDCollectionDate,Community,TotalCover,Latitude,Longitude,SpecCode,Cover)%>%
  pivot_wider(names_from = SpecCode, values_from = Cover,values_fill = 0)%>%
  arrange(StationID,Year)%>%
  select(-Water_BareGround)
veg7$SummedCover<-rowSums(veg7[,which(colnames(veg7)=="Acer_negundo"):which(colnames(veg7)=="Zizip_Mill.")])
veg7$Richness<-specnumber(veg7[,which(colnames(veg7)=="Acer_negundo"):which(colnames(veg7)=="Zizip_Mill.")])
veg7$Shannon<-diversity(veg7[,which(colnames(veg7)=="Acer_negundo"):which(colnames(veg7)=="Zizip_Mill.")],index="shannon")
veg7$Simpson<-diversity(veg7[,which(colnames(veg7)=="Acer_negundo"):which(colnames(veg7)=="Zizip_Mill.")],index="simpson")
veg7$InvSimpson<-diversity(veg7[,which(colnames(veg7)=="Acer_negundo"):which(colnames(veg7)=="Zizip_Mill.")],index="invsimpson")

data.frame(veg8[1,])

veg8<-veg7%>%
  relocate(SummedCover:InvSimpson,.after=TotalCover)

sort(veg8$SummedCover)
head(data.frame(veg8))

dim(veg8) #69933 x 678
length(unique(veg6$StationIDYear)) #69933
length(unique(veg6$StationIDCollectionDate)) #69933
length(unique(veg7$StationIDYear)) #69933



##### Make a column of most common community type for each StationID over the years #####

#Note: this StationID has a community type and then we also want a community type for the whole StationFront
temp <- veg8%>%
  group_by(StationID,StationFront,Community) %>% 
  count(StationID)%>%
  pivot_wider(names_from = Community,values_from = n,values_fill = 0)

temp$CommunityStationID<-colnames(temp)[apply(temp,1,which.max)] 
#there are warnings about NA's but it seems to have worked, there is no NA as the max, stationsIDs with NAs were assigned a marsh/swamp type
unique(temp$CommunityStationID)

which(temp$`NA`>0)
data.frame(temp[681:692,])

temp2<-temp%>%select(StationID,CommunityStationID)
veg9<-veg8%>%
  left_join(temp2)%>%
  relocate(CommunityStationID,.after=Community)
data.frame(veg9[1:10,1:17])
View(veg9)




##### Exporting for homework 10 #####
veg10hmwk<-veg9%>%
  select(StationID:Longitude,Spart_alterniflor)%>%
  select(-Community)
head(data.frame(veg10hmwk))
write.csv(veg10hmwk,"/Users/farrer/Dropbox/EmilyComputerBackup/Documents/Teaching/EcologicalAnalysis2/EA2025/labs/Lab10RepeatedMeasures/veg10hmwk.csv",row.names = F)


##### Looking into Panicum and phrag sites for % carbon #####
head(veg9)
ind<-which(veg9$Phrag_australis>0&veg9$Panic_hemitomon>0)
veg9pp<-veg9[ind,]
veg9pp2<-veg9pp%>%
  select(StationID:Longitude, Phrag_australis,Panic_hemitomon)
View(veg9pp2)
unique(veg9pp2$StationFront)

# ind<-which(veg9$Phrag_australis>0&veg9$Sagit_lancifolia>0)
# veg9pp<-veg9[ind,]
# veg9pp2<-veg9pp%>%
#   select(StationID:Longitude, Phrag_australis,Sagit_lancifolia)
# View(veg9pp2)
# unique(veg9pp2$StationID)

#read in OM data
#our pots are 26cm depth so 24 depth is similar
OM<-read.csv("Data/CRMS_Soil21Feb2025.csv",stringsAsFactors = T)

OM2<-OM%>%
  separate_wider_delim(Station.ID,delim = "-",names = c("StationFront",NA),cols_remove = F)%>%
  filter(StationFront%in%unique(veg9pp2$StationFront))%>%
  filter(Sample.Depth..cm.%in%c("0 to 4","4 to 8","8 to 12","12 to 16","16 to 20","20 to 24"))
  
View(OM2)

mean(OM2$Organic.Matter....)
#The mean is 39.5% OM for 0-8
#The mean is 38.7% OM for 0-12
#The mean is 35.2% OM for 0-24

mean(OM2$Bulk.Density..g.cm3.)
#The mean is 0.2066 for 0-8
#The mean is 0.214 for 0-12
#The mean is 0.247 for 0-24

mean(OM2$Soil.Salinity..ppt.,na.rm=T)
#0.82 ppt

#typical bulk density of soil with ~19%SOM (i.e. 11%C)
temp<-OM%>%
  filter(Organic.Matter....>18&Organic.Matter....<20)
View(temp)
mean(temp$Bulk.Density..g.cm3.)

library(sf)
library(tigris)
louisiana_shapefile <- states(cb = TRUE,resolution="20m")#, state = "LA"
plot(louisiana_shapefile)
points(veg9pp2$Longitude,veg9pp2$Latitude)

la_parishes <- counties(state = "LA")
la_state <- states() |>
  filter_state("Louisiana")

ggplot(veg9pp2) +
  geom_sf(data = la_parishes) +
  geom_sf(data = la_state, fill = NA, color = "red") +
  theme_void()+
  geom_point(aes(x=Longitude,y=Latitude))

# library(osmdata)
# louisiana <- opq(getbb("Louisiana"))
# parishes <- osmdata_sf(add_osm_feature(louisiana, key = "admin_level", value = "6")) #6 is counties, 4 is state, but bounding box is odd: https://stackoverflow.com/questions/78190635/why-do-i-get-neighboring-states-when-mapping-in-r-with-osmdata
# ggplot(veg9pp2) +
#   geom_sf(data = parishes$osm_multipolygons) +
#   theme_void()+
#   geom_point(aes(x=Longitude,y=Latitude))


#Sagittaria and phrag sites
head(veg9)
ind<-which(veg9$Phrag_australis>0&veg9$Sagit_lancifolia>0)
veg9sp<-veg9[ind,]
veg9sp2<-veg9sp%>%
  select(StationID:Longitude, Phrag_australis,Sagit_lancifolia)
View(veg9sp2)
unique(veg9sp2$StationFront)

dim(veg9pp)

ggplot(veg9sp2) +
  geom_sf(data = la_parishes) +
  geom_sf(data = la_state, fill = NA, color = "red") +
  theme_void()+
  geom_point(aes(x=Longitude,y=Latitude))

OM3<-OM%>%
  separate_wider_delim(Station.ID,delim = "-",names = c("StationFront",NA),cols_remove = F)%>%
  filter(StationFront%in%unique(veg9sp2$StationFront))%>%
  filter(Sample.Depth..cm.%in%c("0 to 4","4 to 8","8 to 12","12 to 16","16 to 20","20 to 24"))

head(data.frame(OM3))
mean(OM3$Soil.Salinity..ppt.,na.rm=T)
#1.0 ppt


#Eleocharis cellulosa and phrag sites
ind<-which(veg9$Panic_hemitomon>0&veg9$Eleoc_cellulosa>0)#Phrag_australis
veg9ep<-veg9[ind,]
veg9ep2<-veg9ep%>%
  select(StationID:Longitude, Panic_hemitomon,Eleoc_cellulosa)
View(veg9ep2)
unique(veg9ep2$StationFront)

#Spartina patens and phrag sites
ind<-which(veg9$Phrag_australis>0&veg9$Spart_patens>0)
veg9ps<-veg9[ind,]
veg9ps2<-veg9ps%>%
  select(StationID:Longitude, Phrag_australis,Spart_patens)
View(veg9ps2)
unique(veg9ps2$StationFront)

#our pots are 26cm depth so 24 depth is similar
OM<-read.csv("Data/CRMS_Soil21Feb2025.csv",stringsAsFactors = T)

OM2<-OM%>%
  separate_wider_delim(Station.ID,delim = "-",names = c("StationFront",NA),cols_remove = F)%>%
  filter(StationFront%in%unique(veg9ps2$StationFront))%>%
  filter(Sample.Depth..cm.%in%c("0 to 4","4 to 8","8 to 12","12 to 16","16 to 20","20 to 24"))

View(OM2)

mean(OM2$Organic.Matter....,na.rm=T)
#The mean is 34.8% OM for 0-24

mean(OM2$Bulk.Density..g.cm3.,na.rm=T)
#The mean is 0.254 for 0-24

mean(OM2$Soil.Salinity..ppt.,na.rm=T)
#2.60 ppt


#Just where is phrag
ind<-which(veg9$Phrag_australis>0)
veg9p<-veg9[ind,]
veg9p2<-veg9p%>%
  select(StationID:Longitude, Phrag_australis)
View(veg9p2)
unique(veg9p2$StationFront)

#our pots are 26cm depth so 24 depth is similar
OM<-read.csv("Data/CRMS_Soil21Feb2025.csv",stringsAsFactors = T)

OM2<-OM%>%
  separate_wider_delim(Station.ID,delim = "-",names = c("StationFront",NA),cols_remove = F)%>%
  filter(StationFront%in%unique(veg9p2$StationFront))%>%
  filter(Sample.Depth..cm.%in%c("0 to 4","4 to 8","8 to 12","12 to 16","16 to 20","20 to 24"))

View(OM2)

mean(OM2$Organic.Matter....,na.rm=T)
#The mean is 32.1% OM for 0-24

mean(OM2$Bulk.Density..g.cm3.,na.rm=T)
#The mean is 0.30 for 0-24

mean(OM2$Soil.Salinity..ppt.,na.rm=T)
#2.10 ppt








##### Select the years and plots you want. only include plots that were started in 2006-2009 and ended in 2020-2023 #####
temp<-veg9%>%
  group_by(StationID)%>%
  summarize(minyear=min(Year),maxyear=max(Year))%>%
  filter(minyear<2010)%>%
  filter(maxyear>2019)

veg10<-veg9%>%
  filter(StationID%in%temp$StationID)%>%
  filter(Year<2024)

#Then filter out ones with fewer than 6 time points
temp<-veg10%>%
  group_by(StationID)%>%
  count(StationID)
data.frame(temp)
min(temp$n) #the minimum is 10, so no need to filter

View(veg10)



##### Add a community type at the StationFront level #####

#Rerun the most common community type code again to do it by StationFront (or for the first half of the years and second half of the years??). merge with above. (it might be better to calculate the CommunityStationFront on the individual "Community" variable rather than on the CommunityStationID variable, since there is a lot of variability over time at a StationID). it might also be better rerun the CommunityStationID if you are deleting years. but I don't know if we even need CommunityStationID

temp <- veg10%>%
  group_by(StationFront,Community) %>% 
  count(StationFront)%>%
  pivot_wider(names_from = Community,values_from = n,values_fill = 0)

temp$CommunityStationFront<-colnames(temp)[apply(temp,1,which.max)] 
#there are warnings about NA's but it seems to have worked, there is no NA as the max, stationsIDs with NAs were assigned a marsh/swamp type
unique(temp$CommunityStationFront)

which(temp$`NA`>0)
data.frame(temp[54:56,])
data.frame(temp[311:313,])

temp2<-temp%>%select(StationFront,CommunityStationFront)
veg11<-veg10%>%
  left_join(temp2)%>%
  relocate(CommunityStationFront,.after=CommunityStationID)
data.frame(veg11[1:10,1:17])
View(veg11)

data.frame(veg11)[1:10,1:25]


##### Create a StationFront*year-level dataset by summarizing means across speciescover/diversity by stationfront and year. And sum unknowns into one column #####

# Sum the unknowns into one "unknown" column. I will wait on this, not do it for now b/c if I want to recalculate richness or diversity at an entire StationFront I need the unknowns separated

veg12<-veg11%>%
  mutate(Unknown=rowSums(dplyr::select(veg11, starts_with("Unkno"))))%>%
  dplyr::select(-starts_with("Unkno_"))%>%
  group_by(StationFront,CommunityStationFront)%>%
  summarise_at(vars(TotalCover:Unknown),mean,na.rm=T)
  
#data.frame(veg12)[620:630,630:685]
#data.frame(veg12)[620:630,636:676]
veg12$Unknown
colnames(veg12)
data.frame(veg12)[1:10,1:40]
dim(veg12)



##### Merge veg and acc and basins#####
colnames(veg12)
colnames(acc5)

dim(veg12) #378
dim(acc5) #258
dim(dat) #255, not bad! only lost 3 stationfronts wow

datwithbasins<-read.csv("Data/dat with basins.csv")

datwithbasins2<-datwithbasins%>%
  select(StationFront,Basin)

dat<-veg12%>%
  inner_join(acc5)%>%
  relocate(estabdate:lon,.after=StationFront)%>%
  rename(Accretion=acc)%>%
  left_join(soil5)%>%
  relocate(BelowgroundLive:BelowgroundDead,.after=lon)%>%
  left_join(datwithbasins2)%>%
  relocate(Basin,.after=StationFront)
  
head(dat)
dim(dat) #255 rows, 678 cols

##### Merge dat with basins #####
write.csv(dat,"Data/dat.csv",row.names = F)






##### Things that haven't been done yet #####

#Replace richness, shannon, and tot, if you want to recalculate them based on the new averaged stationfront-level species data (rather than have them be the average across the small plots). I think I did this before when I analyzed accretion/diversity, but now that I think about it more, since accretion is measured on such a small scale (little core), the plot level metrics might be better reprentative of the effects on accretion than a big site level richness
veg6$richness<-specnumber(veg6[,8:437])
veg6$shannon<-diversity(veg6[,8:437],index="shannon")
veg6$tot<-rowSums(veg6[,8:437])

#Then create a StationFront-level dataset (average over years)
veg7<-veg6%>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(CoverTotal:ZiziMill),mean,na.rm=T)









