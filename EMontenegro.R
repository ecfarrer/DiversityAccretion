
##uploading data##
dat <- read.csv("Data/dat.csv",stringsAsFactors = T)
head(dat)
##Split the data into east/west: chenier plain (west of longitude -92) and mississippi delta (east of lon -92)##
library(dplyr)
dat1 <- dat %>%
  mutate(Region = ifelse(Longitude < -92, "Chenier Plain (west)", "Mississippi Delta (east)"))
str(dat1)
##Richness vs. community type and east/west, boxplot with figure legend##
library(ggplot2)
ggplot(dat1,aes(x=CommunityStationFront,y=Richness, fill=Region)) +
  geom_boxplot() +
  labs(
    title = "Species Richness by Community Type and Region",
    x = "Community Type",
    y = "Species Richness",
    fill = "Region"
  )
##glm including spatial autocorrelation, type III anova##
library(nlme)
dat2 <- dat1 %>%
  filter(CommunityStationFront != "Swamp") %>%
  mutate(CommunityStationFront = droplevels(CommunityStationFront))
m1 <- lme(Richness ~ CommunityStationFront + Region + CommunityStationFront * Region,
          random = ~1 | Region,
          correlation = corSpher(form = ~ lat + lon),
          data = dat2)
summary(m1)
m3 <- anova(m1,type="marginal")
m3

