setwd("C:/Users/kolog/OneDrive/Desktop/DiversityAccretion")
CRMS_dat <- read.csv("dat.csv", stringsAsFactors = TRUE)
CRMS_Accretion <- read.csv("CRMS_Accretion.csv", stringsAsFactors = TRUE)
library(tidyverse)
library(ggplot2)
library(plotrix)
#clean data
dat_clean <- CRMS_dat %>%
  filter(!is.na(Accretion)) %>%
  mutate(Community = factor(CommunityStationFront, levels = c("Swamp", "Freshwater", "Intermediate", "Brackish", "Saline"))) %>%
  group_by(Community)

ggplot(CRMS_dat, aes(x = CommunityStationFront, y = Richness, fill = CommunityStationFront)) +
  geom_boxplot() +
  labs(title = "Box Plot of Diversity Metrics by Community Station Front") +
  theme_minimal()

ggplot(CRMS_dat, aes(x = CommunityStationFront, y = Shannon, fill = CommunityStationFront)) +
  geom_boxplot() +
  labs(title = "Box Plot of Diversity Metrics by Community Station Front") +
  theme_minimal()

ggplot(CRMS_dat, aes(x = CommunityStationFront, y = Simpson, fill = CommunityStationFront)) +
  geom_boxplot() +
  labs(title = "Box Plot of Diversity Metrics by Community Station Front") +
  theme_minimal()
ggplot(CRMS_dat, aes(x = CommunityStationFront, y = TotalCover, fill = CommunityStationFront)) +
  geom_boxplot() +
  labs(title = "Box Plot of Diversity Metrics by Community Station Front") +
  theme_minimal()
ggplot(CRMS_dat, aes(x = CommunityStationFront, y = SummedCover, fill = CommunityStationFront)) +
  geom_boxplot() +
  labs(title = "Box Plot of Diversity Metrics by Community Station Front") +
  theme_minimal()
ggplot(CRMS_dat, aes(x = Accretion,  fill = CommunityStationFront)) +
  geom_boxplot() +
  labs(title = "Box Plot of Accretion Metrics by Community Station Front") +
  theme_minimal()
