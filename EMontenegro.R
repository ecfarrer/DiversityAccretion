install.packages("gitcreds")
library(gitcreds)
gitcreds_set() #enter your personal access token
##uploading data##
dat <- read.csv("Data/dat.csv",stringsAsFactors = T)
head(dat)
##Split the data into east/west: chenier plain (west of longitude -92) and mississippi delta (east of lon -92)##
library(dplyr)
dat <- dat %>%
  mutate(Region = ifelse(Longitude < -92, "Chenier Plain", "Mississippi Delta"))
dat
##Richness vs. community type and east/west, boxplot with figure legend##
library(ggplot2)
ggplot(dat,aes(x=CommunityStationFront,y=Richness, fill=Region)) +
  geom_boxplot()
##glm including spatial autocorrelation, type III anova##
library(nlme)
m1 <- lme(Richness ~ CommunityStationFront + Region, random=~1|Region, correlation=corSpher(form = ~ lat+lon),  data = dat) 
summary(m1)
anova(m1,type="marginal")

