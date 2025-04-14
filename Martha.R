library(tidyverse)
library(nlme)

data <- read_csv("dat.csv")

brackish <- data %>%
  filter(CommunityStationFront == "Brackish")

#dominant species - Spartina patens
dom_spp <- brackish %>%
  summarise(across(c("Acer_negundo":"Zizip_Mill."), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = c("Acer_negundo":"Zizip_Mill."),
               names_to = "species",
               values_to = "cover")


#accretion vs. richness
ggplot(brackish, aes(x = Richness, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

#accretion vs. dominant species
ggplot(brackish, aes(x = Spart_patens, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

#accretion vs. summed cover
ggplot(brackish, aes(x = SummedCover, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)


#glm 
m1 <- gls(Accretion ~ Richness + Spart_patens + SummedCover, data = brackish)

m2 <- gls(Accretion ~ Richness + Spart_patens + SummedCover, 
          correlation = corSpher(form = ~ lat+lon), data = brackish)
  
anova(m1, m2)

anova(m2, type = "marginal")
anova(m1, type = "marginal")
