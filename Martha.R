library(tidyverse)
library(vegan)

data <- read_csv("dat.csv")

#removing species with ≤2 non-zero values
non_zero_counts <- colSums(data != 0)
filtered_data <- data[, non_zero_counts >= 3]


#removing non-species data
species <- filtered_data %>%
  subset(select = Acer_rubrum:Unknown)

nmds_result <- metaMDS(species, distance = "bray", k = 2, trymax = 100)
nmds_result$stress


plot(nmds_result, type = "t")

