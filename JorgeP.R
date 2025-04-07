#Jorge's code
install.packages("gitcreds")
library(gitcreds)
gitcreds_set()

library(ggplot2)
library(tidyverse)
library(dplyr)
library(MASS)


Data <-read.csv("~/Documents/Ecological_Analysis/DiversityAccretion/Data/dat.csv")
head(Data)
options(contrasts=c("contr.helmert","contr.poly"))
plot(Data$Accretion, Data$TotalCover )
M1 <- glm(Accretion ~ Richness + TotalCover, data = Data)
summary(M1)
#redo with community in mind