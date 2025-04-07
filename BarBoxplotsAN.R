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



