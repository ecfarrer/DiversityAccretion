2+2
#test file
5+9

dat <- read.csv("dat.csv")
#make a boxplot of richness (x axis) vs accretion (y axis)

library(ggplot2)

ggplot(dat, aes(x = Richness, y = Accretion)) +
  geom_point() +
  facet_wrap(~ CommunityStationFront) +  # Creates separate panels for each community type
  labs(x = "Richness", y = "Accretion", 
       title = "Accretion by Richness, Split by Community Type") +
  theme_minimal()
