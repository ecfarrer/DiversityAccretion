
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
##glm including spatial autocorrelation
library(nlme)
dat2 <- dat1 %>%
  filter(CommunityStationFront != "Swamp")
options(contrasts=c("contr.treatment","contr.poly"))
m1 <- 
  gls(Richness ~ CommunityStationFront + Region + CommunityStationFront * Region,
          correlation = corSpher(form = ~ lat + lon),
          data = dat2)
summary(m1)

##type III anova##
m3 <- anova(m1,type="marginal")
m3

##Belowground biomass live vs. community type * east/west, boxplot with figure legend##
dat_up <- read.csv("Data/dat(1).csv",stringsAsFactors = T)
head(dat1)

dat_up1 <- dat_up %>%
  mutate(Region = ifelse(Longitude < -92, "Chenier Plain (west)", "Mississippi Delta (east)")) %>%
  filter(!is.na(BelowgroundLive))
str(dat_up1)

ggplot(dat_up1,aes(x=CommunityStationFront,y=BelowgroundLive, fill=Region)) +
  geom_boxplot() +
  labs(
    title = "Belowground Biomass by Community Type and Region",
    x = "Community Type",
    y = "Species Richness",
    fill = "Region"
  )
##glm including spatial autocorrelation##

options(contrasts=c("contr.treatment","contr.poly"))
mod1 <- 
  gls(BelowgroundLive ~ CommunityStationFront + Region + CommunityStationFront * Region,
      correlation = corSpher(form = ~ lat + lon),
      data = dat_up1)
summary(mod1)

##type III anova##

mod2 <- anova(mod1,type="marginal")
mod2
