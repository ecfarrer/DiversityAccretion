library(tidyverse)

#testpush 
plants <- c("Acer", "Bacharis", "Elm", "Saltbush")
numbers <- c(1,2,1.2,2.2)            


#bray-curtis
library(vegan)
dat_accr <- read.csv("Data/CRMS_Accretion.csv")
dat < read.csv("Data/dat.csv")

#phrag <- dat %>%
 # subset(dat, Accretion, Phrag_australis, drop = FALSE)


spe.trim<-dat%>%
  select(where(~ sum(.) >= 100))

species_scores2<-data.frame(spe.trim$SummedCover,
                            labels=rownames(spe.nmds$species))
site_scores2 <- data.frame(env,spe.nmds$points,
                           labels=rownames(spe.nmds$points))
site_scores2$eutrophied<-c(rep("eutrophied",15),rep("oligotrophic",14))

ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab("NMDS1") + 
  ylab("NMDS2") +  
  geom_text(data=site_scores2, aes(x=MDS1, y=MDS2, label=labels), size=6,
            color=ifelse(site_scores2$eutrophied=="eutrophied",2,3)) +
  geom_text(data=species_scores2, aes(x=MDS1, y=MDS2, label=labels),size=5)

