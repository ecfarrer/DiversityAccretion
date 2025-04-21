#Jorge's code
#install.packages("gitcreds")
library(gitcreds)
#gitcreds_set()

library(ggplot2)
library(tidyverse)
library(dplyr)
library(MASS)
library(vegan)
library(ggrepel)

Data <-read.csv("~/Documents/Ecological_Analysis/DiversityAccretion/Data/dat.csv", row.names = 1)
head(Data)
options(contrasts=c("contr.helmert","contr.poly"))



unique(Data$CommunityStationFront)
BrackishCommunity <- Data %>% filter(CommunityStationFront %in% c("Brackish"))
head(BrackishCommunity)
SalineCommunity <- Data %>% filter(CommunityStationFront %in% c("Saline"))
head(SalineCommunity)
SalineCommunity <- Data %>% filter(CommunityStationFront %in% c("Saline"))
IntermediateCommunity <- Data %>% filter(CommunityStationFront %in% c("Intermediate"))
head(IntermediateCommunity)
SwampCommunity <- Data %>% filter(CommunityStationFront %in% c("Swamp"))
FreshwaterCommunity <- Data %>% filter(CommunityStationFront %in% c("Freshwater"))
#ok all ive done is make a bunch of subest data frames with just one communities data in it
plot(BrackishCommunity$Accretion, BrackishCommunity$TotalCover )
plot(SalineCommunity$Accretion, SalineCommunity$TotalCover)
plot(IntermediateCommunity$Accretion, IntermediateCommunity$TotalCover)
plot(SwampCommunity$Accretion, SwampCommunity$TotalCover)
plot(FreshwaterCommunity$Accretion, FreshwaterCommunity$TotalCover)
#plotting to check for heteroscedastidy, all starry sky

M1 <- glm(Accretion ~ Richness + TotalCover, data = BrackishCommunity)
summary(M1)
M2 <- glm(Accretion ~ Richness + TotalCover, data = SalineCommunity)
summary(M2)
#playing around with models

PC_Data <- rda(Data, scale = TRUE)
#i need to have a data frame with just numeric type characters so gotta make a new data frame
#trying to do an exploratory PCA
#its mad at me
#have to make a data frame of the names
NumbersData <- Data[-c(1,2,7,13)]
#this got rid of the columns corresponding to these numbers since they were causing problems because
#they were not numeric characters
PC_Data <- rda(NumbersData, scale= TRUE)
#now this works!
summary(PC_Data)
#ok got it working so now i just need to find a way to turn this into a PCA!\
head(NamesData)


#ok scratch everything its a PCoA we need to make
PCoAData<-capscale(NumbersData~1,distance="bray")
summary(PCoAData)
scores(PCoAData)
#species data nested in other data!
species_scores<-data.frame(scores(PCoAData)$species,
                           labels=rownames(scores(PCoAData)$species))
site_scores <- data.frame(cbind(env,scores(PCoAData)$sites,
                                labels=rownames(scores(PCoAData)$sites)))
ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab("MDS1") + 
  ylab("MDS2") +  
  geom_text(data=site_scores, aes(x=MDS1, y=MDS2, label=labels), size=6) +
  geom_segment(data=species_scores, aes(x=0, y=0, xend=MDS1, yend=MDS2), 
               colour="red", size=0.4, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scores, aes(x=MDS1, y=MDS2, label=labels),
                  size=5,colour="red",max.overlaps = 23)
#ok the code is working but i need to make it colored and based on community station ID