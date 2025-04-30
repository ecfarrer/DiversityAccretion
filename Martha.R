library(tidyverse)
library(nlme)
library(patchwork)
library(piecewiseSEM)
library(MASS)

data <- read_csv("Data/dat.csv")


brackish <- data %>%
  filter(CommunityStationFront == "Brackish") %>%
  mutate(Region = case_when(lon < -92 ~ "west",
                            lon > -92 ~ "east"))

brackish_reduced <- na.omit(brackish)
  

#dominant species - Spartina patens
dom_spp <- brackish %>%
  summarise(across(c("Acer_negundo":"Zizip_Mill."), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = c("Acer_negundo":"Zizip_Mill."),
               names_to = "species",
               values_to = "cover")


#accretion vs. richness
richness <- ggplot(brackish, aes(x = Richness, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)


#accretion vs. summed cover
sum_cov <- ggplot(brackish, aes(x = SummedCover, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

(richness | sum_cov)


#accretion vs. region
region <- ggplot(brackish, aes(x = Region, y = Accretion)) +
  geom_boxplot()


#accretion vs. dominant species
dominant1 <- ggplot(brackish, aes(x = Spart_patens, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

dominant2 <- ggplot(brackish, aes(x = Disti_spicata, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

dominant3 <- ggplot(brackish, aes(x = Spart_alterniflor, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

dominant4 <- ggplot(brackish, aes(x = Schoe_americanus, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

(dominant1 | dominant2) / (dominant3 | dominant4)



#accretion vs. below-ground biomass
below_live <- ggplot(brackish_reduced, 
                     aes(x = BelowgroundLive, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

below_dead <- ggplot(brackish_reduced,
                     aes(x = BelowgroundDead, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

(below_live | below_dead)


#glm with reduced data set (for biomass analysis - below ground biomass data available for only 25 out of total 47 station fronts (data = brackish_reduced))

options(constrasts = c("contr.helmert", "contr.poly"))

m1 <- gls(Accretion ~ Richness + SummedCover + BelowgroundLive + 
            BelowgroundDead + Region + Spart_patens, 
          correlation = corSpher(form = ~ lat+lon), 
          method = "ML", data = brackish_reduced)

stepAIC(m1, direction = "backward") 

m2 <- gls(Accretion ~ Richness + SummedCover + Region + Spart_patens, 
          correlation = corSpher(form = ~ lat+lon), 
          method = "REML", data = brackish_reduced)

anova(m2, type = "marginal")

#glm with full data set (excludes biomass analysis since biomass was removed in the previous model selection (data = brackish))
m3 <- gls(Accretion ~ Richness + SummedCover + Region + Spart_patens, 
          correlation = corSpher(form = ~ lat+lon), 
          method = "ML", data = brackish)

stepAIC(m3, direction = "backward")

#THIS IS THE FINAL MODEL
m4 <- gls(Accretion ~ Richness, 
          correlation = corSpher(form = ~ lat+lon), 
          method = "REML", data = brackish)

anova(m4, type = "marginal")


#model validation - assumptions of normality and homoscedasticity met
plot(x = fitted(m4), y = resid(m4, type = "normalized"), abline(h = 0))

hist(resid(m4, type = "normalized"))


#extracting R-squared
rsquared(m4)





