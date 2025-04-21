#Exploratory Plots April 7, 2025
#Diversity and community types (bar/box plot)
dat <- read.csv("Data/dat.csv", header = TRUE)
library(tidyverse)
library(plotrix)
library(ggplot2)

#Cleaning data up
cleandat <- dat %>% filter(!is.na(Accretion)) %>%
  mutate(Community=factor(CommunityStationFront, levels=c("Swamp","Freshwater","Intermediate","Brackish","Saline")))%>%
  group_by(Community)
boxplot(Richness~Community,data = cleandat, xlab = "Community", ylab = "Diversity")
boxplot(Shannon~Community,data = cleandat, xlab = "Community", ylab = "Shannon Diversity")
boxplot(Simpson~Community,data = cleandat, xlab = "Community", ylab = "Simpson Diversity") 

#barplots
ggplot(cleandat, aes(x = Community, y = Richness)) +
  geom_bar(stat = "identity") +
  labs(title = "Diversity by Community type", x = "Community", y = "Richness")

ggplot(cleandat, aes(x = Community, y = Shannon)) +
  geom_bar(stat = "identity") +
  labs(title = "Diversity by Community type", x = "Community", y = "Shannon_Richness")

ggplot(cleandat, aes(x = Community, y = Simpson)) +
  geom_bar(stat = "identity") +
  labs(title = "Diversity by Community type", x = "Community", y = "Simpson_Richness")

#Accretion and community type (bar/box)
boxplot(Accretion~Community,data = cleandat, xlab = "Community", ylab = "Accretion")
