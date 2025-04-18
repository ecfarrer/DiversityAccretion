# Emma Ortega
install.packages("gitcreds") 
library(gitcreds)
gitcreds_set()
library(tidyverse) 
library(nlme)
library(piecewiseSEM)

data <- read.csv("dat.csv", stringsAsFactors = T)

### April 7
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

ggplot(data,aes(x=Accretion,y=Richness,color=CommunityStationFront))+ 
        geom_point(position = "jitter")+
        labs(x = "Accretion",y="Richness")+ 
        geom_line(stat="smooth",method = "lm",size=.9)+ 
        facet_wrap(~communitytype)

### April 9
# Total/ Summed cover on accretion
ggplot(data,aes(x=Accretion,y=SummedCover,color=CommunityStationFront))+ 
        geom_point(position = "jitter")+
        labs(x = "Accretion",y="Summed Cover")+ 
        geom_line(stat="smooth",method = "lm",size=.9, col = "black")+ 
        ggtitle("Summed Cover on Accretion") + 
        facet_wrap(~communitytype)

ggplot(data,aes(x=Accretion,y=TotalCover,color=CommunityStationFront))+ 
        geom_point(position = "jitter")+
        labs(x = "Accretion",y="Total Cover")+ 
        geom_line(stat="smooth",method = "lm",size=.9, col = "black")+ 
        ggtitle("Total Cover on Accretion") + 
        facet_wrap(~communitytype)

colSums(Filter(is.numeric, data))
data %>%
        select_if(is.numeric) %>%
        map_dbl(sum)

### April 14

# Separate east vs west
data2 <- data %>%
        mutate(east_west = case_when(Longitude < (-92) ~ "chenier_plain",
                                     Longitude > (-92) ~ "ms_delta"))
#remove 'swamp' for model
data3 <- data2 %>%
        filter(CommunityStationFront == "Freshwater" | CommunityStationFront == "Intermediate" | CommunityStationFront == "Brackish" | CommunityStationFront == "Saline")

#figure for cover vs community type by location
ggplot(data2,aes(x=communitytype,y=SummedCover, fill = communitytype))+ 
        geom_boxplot() + 
        labs(x = "Community Type",y="Summed Cover")+
        ggtitle("Summed Cover vs Community Type by Location") + 
        theme_light() + scale_fill_brewer(palette="Dark2") + 
        facet_wrap(~east_west)

# model
options(contrasts=c("contr.helmert","contr.poly"))
model <- gls(SummedCover ~ CommunityStationFront*east_west, correlation=corSpher(form = ~ lat+lon), data=data3)
summary(model)
anova(model, type = "marginal")

#R2
rsquared(model)
