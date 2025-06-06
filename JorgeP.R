#Jorge's code
#install.packages("gitcreds")
library(gitcreds)
#gitcreds_set()
#rm(list = ls())


library(vegan)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tibble)

Data <-read.csv("~/Documents/Ecological_Analysis/DiversityAccretion/Data/dat.csv", row.names = 1)
data2<- read.csv("~/Documents/Ecological_Analysis/DiversityAccretion/Data/dat.csv")
#can also read in like this to get the first collumn as the row names: 
#Data<-read.csv("~/Documents/Ecological_Analysis/DiversityAccretion/Data/dat.csv", row.names = 1)

communities <- c("Swamp", "Brackish", "Intermediate", "Saline", "Freshwater")

for (comm in communities) {cat("\n--- Now processing:", comm, "---\n")}
#filter by community
sub_data <- Data %>% filter(CommunityStationFront == comm)
env_meta_cols <- c("estabdate", "enddate", "Accretion", "n", "lat", "lon",
                   "CommunityStationFront", "TotalCover", "SummedCover",
                   "Richness", "Shannon", "Simpson", "InvSimpson",
                   "Latitude", "Longitude")

species_data <- sub_data %>%
  select(where(is.numeric)) %>%
  select(-all_of(env_meta_cols)) 

numeric_data <- select(sub_data, where(is.numeric))
species_data <- select(numeric_data, -all_of(env_meta_cols))
select
# grab numeric columns (includes species + env)
# exclude env/metadata
# Separate species (assumes species are all numeric and not the env or community columns)
sapply(sub_data, class)
##########















head(Data)

options(contrasts=c("contr.helmert","contr.poly"))



unique(Data$CommunityStationFront)

BrackishCommunity <- Data %>% filter(CommunityStationFront %in% c("Brackish"))
#makes the data filtered by just the brackish communities
head(BrackishCommunity)
#sample_ids_Brackish <- BrackishCommunity$StationFront
#reccords the first column called Station front

Brackish_species <- BrackishCommunity[-c(1,2,7)]
head(Brackish_species)
#getting rid of data that wont be needed anymore like community station front and station fron collumns
#rownames(Brackish_species) <- sample_ids_Brackish
#makes the station columns reccorded before into the rownames of the data table could have
#skipped this step by reading in the data with the first row as the row names but now doing it manually
#i decided to just read in the data with the first column as the row names to save time

SalineCommunity <- Data %>% filter(CommunityStationFront %in% c("Saline"))
head(SalineCommunity)
Saline_species <- SalineCommunity[-c(1,2,7)]
head(Saline_species)

IntermediateCommunity <- Data %>% filter(CommunityStationFront %in% c("Intermediate"))
head(IntermediateCommunity)
Intermediate_species <- IntermediateCommunity[-c(1,2,7)]

SwampCommunity <- Data %>% filter(CommunityStationFront %in% c("Swamp"))
Swamp_species <- SwampCommunity[-c(1,2,7)]

FreshwaterCommunity <- Data %>% filter(CommunityStationFront %in% c("Freshwater"))
Freshwater_species <- FreshwaterCommunity[-c(1,2,7)]

#ok all ive done is make a bunch of subset data frames with just one communities data in it
#then removed redundant data and left the data frame with just numeric type characters, only numbers

#now let compute the bray curtis distances 
swamp_dist <- vegdist(Swamp_species, method = "bray")
brackish_dist <- vegdist(Brackish_species, method = "bray")
intermediate_dist <- vegdist(Intermediate_species, method = "bray")
freshwater_dist <- vegdist(Freshwater_species, method = "bray")
saline_dist <- vegdist(Saline_species, method = "bray")
#now lets do the PCoA analysis
swamp_pcoa <- cmdscale(swamp_dist, eig = TRUE, k = 2)
#####################################################################
#got this error: 
#Error in cmdscale(swamp_dist, eig = TRUE, k = 2) : 
#NA values not allowed in 'd'
swamp_dist[is.na(swamp_dist)] <- 0
#i had zeros in the data frame and the progam doesnt like it so i have to get rid of them
#first this turns all NAs into zeros
swamp_dist <- swamp_dist[rowSums(swamp_dist) > 0, ]
#this gets rid of rows with all zeros
#doing the previous step got rid of the problem but this returned an error:
#Error in rowSums(swamp_dist) : 
#'x' must be an array of at least two dimensions
#######################################################################
#moving on
swamp_scores <- as.data.frame(swamp_pcoa$points)
colnames(swamp_scores) <- c("PCoA1", "PCoA2")
swamp_scores$StationFront <- rownames(swamp_scores)  # Add the StationFront back
## Merge with metadata if you want to include more info (e.g., Community or StationID)
swamp_meta <- data2[, c("StatoinFront","CommunityStationFront")]  # Add other columns as needed
swamp_scores <- left_join(swamp_scores, swamp_meta, by = "StationFront")
##
eig_vals <- swamp_pcoa$eig
x_var <- round(100 * eig_vals[1] / sum(eig_vals), 1)
y_var <- round(100 * eig_vals[2] / sum(eig_vals), 1)
##
ggplot(swamp_scores, aes(x = PCoA1, y = PCoA2)) +
  geom_point(aes(color = "red"), size = 4) +  
  geom_text_repel(aes(label = StationFront), size = 3) +  # Label points with StationFront
  labs(
    title = "PCoA - Swamp Community",
    x = paste0("PCoA1 (", x_var, "%)"),
    y = paste0("PCoA2 (", y_var, "%)")
  ) +
  theme_minimal()
##

env_data <- Swamp_species[, c("Accretion", "n", "lat", "lon", 
                     "TotalCover", "SummedCover", "Richness",
                     "Shannon", "Simpson", "InvSimpson", 
                     "Latitude", "Longitude")]
env_data_swamp <- env_data[rownames(Swamp_species), ]
env_fit <- envfit(swamp_pcoa, env_data, permutations = 999)
Swamp_species[is.na(Swamp_species)] <- 0
Swamp_species <- Swamp_species[rowSums(Swamp_species) > 0, ]
################################################################################
plot(BrackishCommunity$Accretion, BrackishCommunity$TotalCover )
plot(SalineCommunity$Accretion, SalineCommunity$TotalCover)
plot(IntermediateCommunity$Accretion, IntermediateCommunity$TotalCover)
plot(SwampCommunity$Accretion, SwampCommunity$TotalCover)
plot(FreshwaterCommunity$Accretion, FreshwaterCommunity$TotalCover)
#plotting to check for a relationship between accretion and total cover in each community

M1 <- glm(Accretion ~ Richness + TotalCover, data = BrackishCommunity)
summary(M1)
M2 <- glm(Accretion ~ Richness + TotalCover, data = SalineCommunity)
summary(M2)
#playing around with models

#PC_Data <- rda(Data, scale = TRUE)
#i need to have a data frame with just numeric type characters so gotta make a new data frame
#trying to do an exploratory PCA
#its mad at me
#have to make a data frame of the names
#NumbersData <- Data[-c(1,2,7,13)]
#this got rid of the columns corresponding to these numbers since they were causing problems because
#they were not numeric characters
#PC_Data <- rda(NumbersData, scale= TRUE)
#now this works!
#summary(PC_Data)
#ok got it working so now i just need to find a way to turn this into a PCA!\
#head(NamesData)


#ok scratch everything its a PCoA we need to make
#PCoAData<-capscale(NumbersData~1,distance="bray")
#summary(PCoAData)
#scores(PCoAData)
#head(PCoAData)
#species data nested in other data!
#species_scores<-data.frame(scores(PCoAData)$species,
#                           labels=rownames(scores(PCoAData)$species))
#head(species_scores)
#site_scores <- data.frame(cbind(env,scores(PCoAData)$sites,
#                                labels=rownames(scores(PCoAData)$sites)))

#head(site_scores)
#remove(site_scores)
#colnames(site_scores)[1] <- "SampleID"
#trying to label the CRMS IDs
#its bad


#site_scores$StationID <- Data$CommunityStationFront[match(site_scores$labels, Data$SampleID)]
#trying to add names
#head(site_scores)
#head(Data)

#ggplot() + 
#  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
 # geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
#  xlab("MDS1") + 
#  ylab("MDS2") +  
 # geom_text(data=site_scores, aes(x=MDS1, y=MDS2, label=labels), size=3) +
#  geom_segment(data=species_scores, aes(x=0, y=0, xend=MDS1, yend=MDS2), 
 #              colour="red", size=0.4, arrow=arrow(length=unit(.15,"cm"))) +
#  geom_text_repel(data=species_scores, aes(x=MDS1, y=MDS2, label=labels),
 #                 size=3,colour="red",max.overlaps = 23)
#ok the code is working but i need to make it colored and based on community station ID