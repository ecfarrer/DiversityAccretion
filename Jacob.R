2+2
#test file
5+9

dat <- read.csv("dat.csv")
#make a boxplot of richness (x axis) vs accretion (y axis) by community type

library(ggplot2)

ggplot(dat, aes(x = Richness, y = Accretion)) +
  geom_point() +
  facet_wrap(~ CommunityStationFront) +  # Creates separate panels for each community type
  labs(x = "Richness", y = "Accretion", 
       title = "Accretion by Richness, Split by Community Type") +
  theme_minimal()

#Total cover vs accretion by community type
ggplot(dat, aes(x = TotalCover, y = Accretion, color = CommunityStationFront)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ CommunityStationFront) +  
  labs(x = "Total Cover", y = "Accretion", 
       title = "Accretion vs. Total Cover by Community Type") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# Run separate regressions for each community type (total cover0)
install.packages(c("broom"))
library(dplyr)
library(broom)

regression_results_TotalCover <- dat %>%
  group_by(CommunityStationFront) %>%
  do(tidy(lm(Accretion ~ TotalCover, data = .))) %>%  
  left_join(
    dat %>%
      group_by(CommunityStationFront)) %>%
      do(glance(lm(Accretion ~ TotalCover, dat = .))  
      )
    
    print(regression_results_TotalCover)

    
#Summed cover vs accretion by community type
ggplot(dat, aes(x = SummedCover, y = Accretion, color = CommunityStationFront)) +
      geom_point(alpha = 0.7, size = 3) +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~ CommunityStationFront) +  
      labs(x = "SummedCover", y = "Accretion", 
           title = "Accretion vs. Summed Cover by Community Type") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")

#Regression stats on summed cover
regression_results_SummedCover <- dat %>%
  group_by(CommunityStationFront) %>%
  do(tidy(lm(Accretion ~ SummedCover, data = .))) %>%  
  left_join(
    dat %>%
      group_by(CommunityStationFront)) %>%
  do(glance(lm(Accretion ~ SummedCover, dat = .))  
  )

print(regression_results_SummedCover)

#filter out zeros


#Percent cover of plant species (total cover)


#Stats

#percent cover of plant species (summed cover)


#Stats


#type 3 ANOVA using part of Vineeshas code
    
