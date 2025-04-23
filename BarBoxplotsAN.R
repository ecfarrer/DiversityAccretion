crms_accretion<- read.csv("C:/Users/annik/OneDrive - Tulane University/Documents/Ecological Analysis/CRMS_Accretion.csv", header=TRUE)

crms_dat <- read.csv("C:/Users/annik/OneDrive - Tulane University/Documents/Ecological Analysis/crmsdata.csv", header=TRUE)

library(ggplot2)

crms_dat$CommunityStationFront <- factor(crms_dat$CommunityStationFront, levels= c("Swamp","Freshwater", "Intermediate", "Brackish", "Saline"))
summary(crms_dat$CommunityStationFront)


ggplot(crms_dat, aes(x=CommunityStationFront, y=Accretion, fill=CommunityStationFront)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")


ggplot(crms_dat, aes(x=CommunityStationFront, y=Richness, fill=CommunityStationFront)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")



ggplot(crms_dat, aes(x=CommunityStationFront, y=Shannon, fill=CommunityStationFront)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

ggplot(crms_dat, aes(x=CommunityStationFront, y=TotalCover, fill=CommunityStationFront)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

ggplot(crms_dat, aes(x=CommunityStationFront, y=SummedCover, fill=CommunityStationFront)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

ggplot(crms_dat, aes(x=Accretion, fill=CommunityStationFront)) + 
  geom_histogram()


ggplot(crms_dat, aes(x=Accretion, fill=CommunityStationFront)) + 
  geom_histogram()+
  facet_wrap(~CommunityStationFront)


ggplot(crms_accretion, aes(x=Sample.Date..mm.dd.yyyy., fill=Accretion.Measurement.1..mm.)) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")




ggplot(crms_accretion, aes(x=Sample.Date..mm.dd.yyyy., y=Accretion.Measurement.1..mm.) ) +
  geom_bin2d() +
  theme_bw()

library(hexbin)
ggplot(crms_accretion, aes(x=Sample.Date..mm.dd.yyyy., y=Accretion.Measurement.1..mm.) ) +
  geom_hex() +
  theme_bw()


library(tidyverse)



salinecrms <- crms_dat %>%
  filter(CommunityStationFront == "Saline")

#Acer_negundo to Unknown

sumsaline <- salinecrms %>%
  select(Acer_negundo:Unknown) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "species", values_to = "total")                     

summary(sumsaline)

#dominant species are S. alterniflora and S. patens 

library(ggplot2)
library(tidyverse)

ggplot(salinecrms, aes(x = Richness, y = Accretion)) +
  geom_point() +                          # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +# Linear regression line with confidence interval
  labs(title = "Richness vs. Accretion",
       x = "Richness",
       y = "Accretion") +
  theme_minimal()




ggplot(salinecrms, aes(x = Spart_alterniflor, y = Accretion)) +
  geom_point() +                          # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +# Linear regression line with confidence interval
  labs(title = "S. alterniflora cover vs. Accretion",
       x = "S. alterniflora cover",
       y = "Accretion") +
  theme_minimal()

salinecrms %>%
  filter(Spart_alterniflor > 0) %>%
  ggplot(aes(x = Spart_alterniflor, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "S. alterniflora cover vs. Accretion",
       x = "S. alterniflora cover",
       y = "Accretion") +
  theme_minimal()


ggplot(salinecrms, aes(x = Spart_patens, y = Accretion)) +
  geom_point() +                          # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +# Linear regression line with confidence interval
  labs(title = "S. patens cover vs. Accretion",
       x = "S. patens cover",
       y = "Accretion") +
  theme_minimal()

#remove zeros 

salinecrms %>%
  filter(Spart_patens > 0) %>%
  ggplot(aes(x = Spart_patens, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "S. patens cover vs. Accretion",
       x = "S. patens cover",
       y = "Accretion") +
  theme_minimal()



ggplot(salinecrms, aes(x = Juncu_roemerianus, y = Accretion)) +
  geom_point() +                          # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +# Linear regression line with confidence interval
  labs(title = "J. roemerianus cover vs. Accretion",
       x = "J. roemerianus cover",
       y = "Accretion") +
  theme_minimal()

#remove zeros 

salinecrms %>%
  filter(Juncu_roemerianus > 0) %>%
  ggplot(aes(x = Juncu_roemerianus, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "J. roemerianus cover vs. Accretion",
       x = "J. roemerianus cover",
       y = "Accretion") +
  theme_minimal()




ggplot(salinecrms, aes(x = Disti_spicata, y = Accretion)) +
  geom_point() +                          # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +# Linear regression line with confidence interval
  labs(title = "D. spicata cover vs. Accretion",
       x = "D. spicata cover",
       y = "Accretion") +
  theme_minimal()

#remove zeros 

salinecrms %>%
  filter(Disti_spicata > 0) %>%
  ggplot(aes(x = Disti_spicata, y = Accretion)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "D. spicata cover vs. Accretion",
       x = "D. spicata cover",
       y = "Accretion") +
  theme_minimal()





ggplot(salinecrms, aes(x = SummedCover, y = Accretion)) +
  geom_point() +                          # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +# Linear regression line with confidence interval
  labs(title = "Summed cover vs. Accretion",
       x = "Summed cover",
       y = "Accretion") +
  theme_minimal()


#glm

library(nlme)

options(contrasts = c("contr.helmert", "contr.poly"))

sal1 <- gls(Accretion~ Richness + Spart_alterniflor + Spart_patens + Juncu_roemerianus + Disti_spicata + SummedCover, correlation = corSpher(form = ~ lat+lon),
              data = salinecrms)

summary(sal1)

anova(sal1, type="marginal")

resids <- residuals(sal1)

hist(resids)
plot(resids, salinecrms$Accretion)


resid_var <- var(residuals(sal1))

resp_var <- var(salinecrms$Accretion)  
r2 <- 1 - (resid_var / resp_var)
r2



## facet 


library(ggplot2)
library(dplyr)
library(tidyr)




ggplot(
  salinecrms %>%
    pivot_longer(cols = c(Richness, SummedCover), names_to = "Predictor", values_to = "Value"),
  aes(x = Value, y = Accretion)
) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Predictor, scales = "free_x") +
  labs(title = "Predictors vs. Accretion",
       x = NULL,
       y = "Accretion") +
  theme_minimal()






ggplot(
  salinecrms %>%
    pivot_longer(cols = c(Spart_alterniflor, Spart_patens, Juncu_roemerianus, Disti_spicata),
                 names_to = "Species", values_to = "Cover") %>%
    filter(Cover > 0) %>%
    mutate(Species = factor(Species, levels = c(
      "Spart_alterniflor", "Spart_patens", "Juncu_roemerianus", "Disti_spicata"
    ))),
  aes(x = Cover, y = Accretion)
) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Species, scales = "free_x", labeller = as_labeller(c(
    "Spart_alterniflor" = "S. alterniflora",
    "Spart_patens" = "S. patens",
    "Juncu_roemerianus" = "J. roemerianus",
    "Disti_spicata" = "D. spicata"
  ))) +
  labs(title = "Species Cover vs. Accretion",
       x = "Species cover",
       y = "Accretion") +
  theme_minimal()
