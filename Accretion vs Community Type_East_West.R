setwd("C:/Users/kolog/OneDrive/Desktop/DiversityAccretion")
CRMS_dat <- read.csv("dat.csv", stringsAsFactors = TRUE)

#Separate the data based on region, East (Chenier_Plain), and West (Mississippi_Delta)
CRMS_dat2 <- CRMS_dat %>% 
  mutate(east_west = ifelse(Longitude < -92, "Chenier_Plain", "Mississippi_Delta"))

#Filter the community type (Swamp) from the data
CRMS_dat3 <- CRMS_dat2 %>% filter(CommunityStationFront %in% c("Freshwater", "Intermediate", "Brackish", "Saline"))

#Box plot of Accretion vs Community type based on the region (east/west)
library(ggplot2)
ggplot(CRMS_dat2, aes(x = CommunityStationFront, y = Accretion, fill = CommunityStationFront)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Accretion vs Community Type",
       x = "Community Type",
       y = "Accretion") +
  facet_wrap(~ east_west, scales = "free") +
  theme_minimal()

#glm including spatial autocorrelation
library(nlme)
library(car)
options(contrasts=c("contr.helmert", "contr.poly"))
glm_model <- gls(Accretion ~ CommunityStationFront * east_west, 
                 correlation = corSpher(form = ~ Longitude + Latitude), 
                 data = CRMS_dat3)
summary(glm_model)
#Anova type III
anova(glm_model, type= "marginal")
summary(Anova)
