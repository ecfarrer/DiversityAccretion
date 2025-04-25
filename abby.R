library(tidyverse)
library(vegan)

dat <- read.csv("/Users/acoldw1/Documents/Grad/EcologicalAnalysis/data/dat.csv") 

# want to drop species that have 0 observations
# only include species data
dat_spe <- dat %>%
  subset(select = Acer_negundo:Unknown) # only species data (cut environmental variables)

non_zero_counts <- colSums(dat_spe != 0) 
filtered_data <- dat_spe[, non_zero_counts >= 3] # keeps columns with 3 or more non-zero values

spe_trimmed <- dat_spe %>%
  select(where(~ sum(.) >= 100)) # refine this cause this filtering method is crazy


nmds<-metaMDS(filtered_data, distance="bray")

stress <- nmds$stress

plot(nmds, type = "t")

# would love to do an nmds colored by community type



## Reworking veg data


