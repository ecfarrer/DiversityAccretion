library(tidyverse)
library(vegan)

dat <- read.csv("/Users/acoldw1/Documents/Grad/EcologicalAnalysis/data/dat.csv") 

# want to drop species that have 0 observations
# only include species data
dat_spe <- dat %>%
  subset(select = Acer_negundo:Unknown)

#non_zero <- dat_spe %>%


spe_trimmed <- dat_spe %>%
  select(where(~ sum(.) >= 100)) # refine this cause this filtering method is crazy


nmds<-metaMDS(spe_trimmed, distance="bray")

plot(nmds, type = "t")
