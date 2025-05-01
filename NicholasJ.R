library(tidyverse)
library(piecewiseSEM)

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
library(MASS)
library(performance)
dat <- read.csv("Data/dat.csv")
Intermediate <- dat %>%
  filter(dat$CommunityStationFront == "Intermediate")
Intermediate2 <- Intermediate %>%
  mutate(Region = case_when(lon < -92 ~ "west",
                            lon > -92 ~ "east"))
cols_to_drop <- c(
  "estabdate", "CommunityStationFront", "enddate", "Accretion", 
  "n", "lat", "lon", "TotalCover", "SummedCover", "Richness", 
  "Shannon", "Simpson", "InvSimpson", "Latitude", "Longitude", "BelowgroundDead", "BelowgroundLive"
)

cols_to_keep <- setdiff(names(Intermediate), cols_to_drop)
dom_species <- Intermediate[, cols_to_keep]
dom_species <- Intermediate %>%
  select(-estabdate, -CommunityStationFront, -enddate, -Accretion, -n, -lat, -lon,-TotalCover,-SummedCover,-Richness, -Shannon, -Simpson, -InvSimpson, -Latitude,-Longitude)



dom_species_clean <- dom_species %>%
  select(StationFront, where(is.numeric)) %>%
  pivot_longer(
    cols = -StationFront,
    names_to = "Species",
    values_to = "Value"
  ) %>%
  drop_na(Value) %>%  # Optional: Remove NA rows
  group_by(StationFront) %>%
  slice_max(Value, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(StationFront, DominantSpecies = Species)








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

avr <- ggplot(data=Intermediate, aes(x=Richness, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle(label = "Accretion v. Richness")

avsc <- ggplot(data=Intermediate, aes(x=SummedCover, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle(label = "Accretion v. Summed Cover")
avrsc <- (avr + avsc)
avrsc
#dom = Spartina patens
patDom <- ggplot(data=Intermediate, aes(x=Spart_patens, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab(label = "Spartina Patens Cover") +
  ggtitle(label = "Accretion v. Spartina patens Cover")

#dom = Vigna luteola
lutDom <- ggplot(data=Intermediate, aes(x=Vigna_luteola, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab(label = "Vigna luteola Cover") +
  ggtitle(label = "Accretion v. Vigna luteola Cover")

#dom = Phrag.
ausDom <- ggplot(data=Intermediate, aes(x=Phrag_australis, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab(label = "Phragmites australis Cover") +
  ggtitle(label = "Accretion v. Phragmites australis Cover")

#dom = Schoenoplectus americanus
ameDom <- ggplot(data=Intermediate, aes(x=Schoe_americanus, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab(label = "Schoenoplectus americanus Cover") +
  ggtitle(label = "Accretion v. Schoenoplectus americanus Cover")

domscombined <- (patDom + lutDom) / (ausDom + ameDom)
domscombined


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
r2



#anovaIII
anova(glm1, type = "marginal")


#april 30 assignment 
#accretion vs. below-ground biomass
ggplot(Intermediate2, aes(x = BelowgroundLive, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

ggplot(Intermediate2, aes(x = BelowgroundDead, y = Accretion)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

Intermediate$Schoe_americanus

#glm with reduced data set (for biomass analysis)
options(constrasts = c("contr.helmert", "contr.poly"))

m1 <- gls(Accretion ~ Richness + SummedCover + BelowgroundLive + 
            BelowgroundDead + Region  + BelowgroundDead + BelowgroundLive +Spart_patens + Vigna_luteola + Phrag_australis + Schoe_americanus, 
          correlation = corSpher(form = ~ lat+lon), 
          method = "ML", data = na.omit(Intermediate2))

stepAIC(m1, direction = "backward") 

m2 <- gls(Accretion ~ Richness + SummedCover + Region + Spart_patens + Vigna_luteola + Phrag_australis + Schoe_americanus, 
          correlation = corSpher(form = ~ lat+lon), 
          method = "REML", data = na.omit(Intermediate2))

anova(m2, type = "marginal")

#glm with all data
m3 <- gls(Accretion ~ Richness + SummedCover + Region + BelowgroundDead + BelowgroundLive +Spart_patens + Vigna_luteola + Phrag_australis + Schoe_americanus, 
          correlation = corSpher(form = ~ lat+lon), 
          method = "ML", data = Intermediate2)

mtest <- gls(Accretion ~ Richness + SummedCover + Region + Spart_patens + Vigna_luteola + Phrag_australis + Schoe_americanus, 
            correlation = corSpher(form = ~ lat+lon), 
            method = "REML", data = Intermediate2)

anova(mtest, type = "marginal")

stepAIC(m3, direction = "backward")

#final accreation~richness
m4 <- gls(Accretion ~ Richness, 
          correlation = corSpher(form = ~ lat+lon), 
          method = "REML", data = Intermediate2)

anova(m4, type = "marginal")


#model validation - normality and homosc. met
plot(x = fitted(m4), y = resid(m4, type = "normalized"), abline(h = 0))

hist(resid(m4, type = "normalized"))


#extracting R-squared
r2(m2)
r2(m4)























