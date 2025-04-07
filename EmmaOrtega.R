# Emma Ortega
install.packages("gitcreds") 
library(gitcreds)
gitcreds_set()
library(tidyverse) 

data <- read.csv("dat.csv", stringsAsFactors = T)

#Recategorise Community Type by salinity
data$communitytype <- factor(data$CommunityStationFront, levels = c("Swamp", "Freshwater", "Intermediate", "Brackish", "Saline"))

#community variable: CommunityStationFront, communitytype (ordered)
#diversity: Richness and Shannon?
#accretion: Accretion

#Richness
plot(data$communitytype, data$Shannon,main="Community Type vs Diversity", xlab="Community Type", ylab="Shannon Diversity Index", pch = 19, col = "#6D9576")
plot(data$communitytype, data$Richness,main="Community Type vs Richness", xlab="Community Type", ylab="Richness", pch = 19, col = "#7FCDBB")
#Accretion
plot(data$communitytype, data$Accretion,main="Community Type vs Accretion", xlab="Community Type", ylab="Accretion", pch = 19, col = "#FC9272")

ggplot(data,aes(x=Accretion,y=Richness,color=communitytype))+ 
        geom_point(position = "jitter")+
        labs(x = "Accretion",y="Richness")+ 
        geom_line(stat="smooth",method = "lm",size=.9)+ 
        facet_wrap(~communitytype)