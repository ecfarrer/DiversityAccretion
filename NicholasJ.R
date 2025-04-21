library(tidyverse)

#bray-curtis
library(vegan)
dat_accr <- read.csv("Data/CRMS_Accretion.csv")
dat <- read.csv("Data/dat.csv")

#phrag <- dat %>%
 # subset(dat, Accretion, Phrag_australis, drop = FALSE)


#removing species with ≤2 non-zero values
non_zero_counts <- colSums(dat != 0)
filtered_data <- dat[, non_zero_counts >= 3]


#removing non-species data
species <- filtered_data %>%
  subset(select = Acer_rubrum:Unknown)

nmds_result <- metaMDS(species, distance = "bray", k = 2, trymax = 100)
nmds_result$stress
#0.1383

plot(nmds_result, type = "t")



library(tidyverse)
library(vegan)
library(nlme)
library(patchwork)

Intermediate <- dat %>%
  filter(dat$CommunityStationFront == "Intermediate")


dom_species <- Intermediate %>%
  select(-estabdate, -CommunityStationFront, -enddate, -Accretion, -n, -lat, -lon,-TotalCover,-SummedCover,-Richness, -Shannon, -Simpson, -InvSimpson, -Latitude,-Longitude)

dom_species <- dom_species %>%
  select(StationFront, where(is.numeric)) %>%  # Select only numeric species columns
  pivot_longer(cols = -StationFront, names_to = "Species", values_to = "Value") %>%
  group_by(StationFront) %>%
  slice_max(Value, n = 1, with_ties = FALSE) %>%  # Get species with highest value
  ungroup() %>%
  select(StationFront, DominantSpecies = Species)
Intermediate <- Intermediate %>%
  add_column(
    dom_species = dom_species$DominantSpecies[
      match(Intermediate$StationFront, dom_species$StationFront)
    ],
    .after = "StationFront"
  )

Intermediate_dom <- Intermediate %>%
  select(Accretion, dom_species) %>%
  distinct() %>%
  group_by(dom_species)

dom5 <- Intermediate_dom %>%
  count(dom_species, sort = TRUE) %>%
  head(5)

ggplot(data=Intermediate, aes(x=Accretion, y = Richness)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(data=Intermediate, aes(x=Accretion, y = SummedCover)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(data=Intermediate, aes(x=SummedCover, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm)

#dom = Spartina patens
ggplot(data=Intermediate, aes(x=Spart_patens, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm)

#dom = Vigna luteola
ggplot(data=Intermediate, aes(x=Vigna_luteola, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm)

#dom = Phrag.
ggplot(data=Intermediate, aes(x=Phrag_australis, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm)

#dom = Schoenoplectus americanus
ggplot(data=Intermediate, aes(x=Schoe_americanus, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm)

#glm of 4 most dom
options(constrasts = c("contr.helmert", "contr.poly"))

glm1 <- gls(Accretion ~ Richness + Spart_patens + Vigna_luteola +
            Phrag_australis + Schoe_americanus + SummedCover, 
          correlation = corSpher(form = ~ lat+lon), data = Intermediate)

summary(glm1)




plot(x = fitted(glm1), y = resid(glm1, type = "normalized"), abline(h = 0))

hist(resid(glm1, type = "normalized"))


glm1r2 <- var(glm1$residuals)
null_model <- gls(Accretion ~ 1, data = Intermediate)
null_var <- var(null_model$residuals)

r2 <- 1- (glm1r2 / null_var)




#anovaIII
anova(glm1, type = "marginal")
