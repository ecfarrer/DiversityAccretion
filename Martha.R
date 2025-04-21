library(tidyverse)
library(nlme)
library(patchwork)
library(piecewiseSEM)

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
richness <- ggplot(brackish, aes(x = Richness, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

#accretion vs. summed cover
sum_cov <- ggplot(brackish, aes(x = SummedCover, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

(richness | sum_cov)

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



#glm with four most dominant species
options(constrasts = c("contr.helmert", "contr.poly"))

m1 <- gls(Accretion ~ Richness + Spart_patens + Disti_spicata +
            Spart_alterniflor + Schoe_americanus + SummedCover, 
          correlation = corSpher(form = ~ lat+lon), data = brackish)

summary(m1)

#model validation - assumptions of normality and homoscedasticity met
plot(x = fitted(m1), y = resid(m1, type = "normalized"), abline(h = 0))

hist(resid(m1, type = "normalized"))


#type III anova - no significant predictor variables 
anova(m1, type = "marginal")


#extracting R-squared
rsquared(m1)


m1_var <- var(m1$residuals)
null_model <- gls(Accretion ~ 1, data = brackish)
null_var <- var(null_model$residuals)
r2 <- 1- (m1_var / null_var)





