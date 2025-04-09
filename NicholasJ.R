library(tidyverse)

#bray-curtis
library(vegan)
dat_accr <- read.csv("Data/CRMS_Accretion.csv")
dat <- read.csv("Data/dat.csv")

#phrag <- dat %>%
 # subset(dat, Accretion, Phrag_australis, drop = FALSE)


#removing species with ≤2 non-zero values
non_zero_counts <- colSums(dat != 0)
filtered_data <- dat[, non_zero_counts >= 3]


#removing non-species data
species <- filtered_data %>%
  subset(select = Acer_rubrum:Unknown)

nmds_result <- metaMDS(species, distance = "bray", k = 2, trymax = 100)
nmds_result$stress
#0.1383

plot(nmds_result, type = "t")
