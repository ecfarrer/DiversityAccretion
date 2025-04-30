library(vegan)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tibble)
library(grid)
library(tidyr)

species_start <- which(colnames(Data) == "Longitude") + 1

species_data <- Data[, species_start:ncol(Data)]

env_data <- Data %>%
  select(Accretion, n, BelowgroundLive, BelowgroundDead, Richness, Shannon, Simpson, TotalCover,
         SummedCover)
#makes a list of all my environmental data
#community <- Data$CommunityStationFront
#just extracting the data in the CommunityStationFront colum
#pca <- rda(species_data, scale = TRUE)
#calculating the PCA stuff
#envfit_pca <- envfit(pca, env_data, permutations = 999)
## got an error so i have to clean some stuff up
combined_data <- cbind(env_data, species_data, CommunityStationFront = community)
clean_data <- combined_data %>% drop_na(Accretion, n, BelowgroundLive, BelowgroundDead, Richness, Shannon, Simpson,
                                        TotalCover, SummedCover)
env_data <- clean_data[, c("Accretion", "n", "BelowgroundLive", "BelowgroundDead", "Richness", "Shannon", "Simpson", "TotalCover", "SummedCover")]
###ok its working now lets redo some steps
species_data <- clean_data[, (ncol(env_data) + 1):(ncol(clean_data) - 1)]

community <- clean_data$CommunityStationFront

pca <- rda(species_data, scale = TRUE)

envfit_pca <- envfit(pca, env_data, permutations = 999)
#####
site_scores <- scores(pca, display = "sites")  # ✅ this is already the matrix you want
pca_df <- as.data.frame(site_scores)
pca_df$Community <- community

arrow_scores <- as.data.frame(scores(envfit_pca, display = "vectors"))
arrow_scores$Variable <- rownames(arrow_scores)

arrow_scale <- 2
arrow_scores <- arrow_scores %>%
  mutate(PC1 = PC1 * arrow_scale,
         PC2 = PC2 * arrow_scale)

ggplot(pca_df, aes(x = PC1, y = PC2, color = Community)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_segment(data = arrow_scores,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "blue", size = 1) +
  geom_text(data = arrow_scores,
            aes(x = PC1 * 1.1, y = PC2 * 1.1, label = Variable),
            color = "blue", size = 4) +
  theme_minimal() +
  labs(title = "PCA with Environmental Vectors",
       x = "PC1", y = "PC2")
#ok its working but now i need to get rid of some outliers
#we can do his by looking inside of the site_scores which holds all the PCA data
#in my case im looking for two points: PC1=12~ and PC2=4~, another PC1=4~, PC2=11~
#i found them! (Emily helped me) they are: CRMS4094 and CRMS0551
#so now i just need to omit them from my master data used to make the PCA, ill just name a new 
#one without any of the data points i want to omit. 
#You can't just remove them from the site_scores object since that will cause problems elsewhere

ind<-which(row.names(Data)%in%c("CRMS4094","CRMS0551"))
#in base R there is a funciton called "which" that can find specifc data for you
#this made an object that has the row numbers which correspond to the ID's we gave (in this case they are also the row names)
Data2<-Data[-ind,]
#next we just removed the corresponding positons from the data frame and made a new data set, easy!
#lets retrace our steps!


species_start <- which(colnames(Data2) == "Longitude") + 1

species_data <- Data2[, species_start:ncol(Data2)]

env_data <- Data2 %>%
  select(Accretion, n, BelowgroundLive, BelowgroundDead, Richness, Shannon, Simpson, TotalCover,
         SummedCover)

combined_data <- cbind(env_data, species_data, CommunityStationFront = Data2$CommunityStationFront)

clean_data <- combined_data %>% drop_na(Accretion, n, BelowgroundLive, BelowgroundDead, Richness, Shannon, Simpson,
                                        TotalCover, SummedCover)
env_data <- clean_data[, c("Accretion", "n", "BelowgroundLive", "BelowgroundDead", "Richness", "Shannon", "Simpson", "TotalCover", "SummedCover")]

species_data <- clean_data[, (ncol(env_data) + 1):(ncol(clean_data) - 1)]

community <- clean_data$CommunityStationFront

pca <- rda(species_data, scale = TRUE)

envfit_pca <- envfit(pca, env_data, permutations = 999)

site_scores <- scores(pca, display = "sites")
pca_df <- as.data.frame(site_scores)
pca_df$Community <- community

arrow_scores <- as.data.frame(scores(envfit_pca, display = "vectors"))
arrow_scores$Variable <- rownames(arrow_scores)

arrow_scale <- 2
arrow_scores <- arrow_scores %>%
  mutate(PC1 = PC1 * arrow_scale,
         PC2 = PC2 * arrow_scale)

ggplot(pca_df, aes(x = PC1, y = PC2, color = Community)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_segment(data = arrow_scores,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "blue", size = 1) +
  geom_text(data = arrow_scores,
            aes(x = PC1 * 1.1, y = PC2 * 1.1, label = Variable),
            color = "blue", size = 4) +
  theme_minimal() +
  labs(title = "PCA with Environmental Vectors",
       x = "PC1", y = "PC2")

#im gonna make a version with just the dots and one with just the arrows now, then im gonna make PCoA's
#within each community

###############################################################################
#ok i have even more outliers so lets get rid of those as well
#PC1 = 3.5~ , PC2 10~ and PC1 = 9, PC2= 4.9
site_scores
#CRMS4014, CRMS4045
#all of the outliers have come from the freshwater community so far

ind<-which(row.names(Data)%in%c("CRMS4094","CRMS0551", "CRMS4014", "CRMS4045"))
ind
Data2<-Data[-ind,]
#im just gonna rerun the code now no more rewriting jus to do the same thing haha
###############################################################################
