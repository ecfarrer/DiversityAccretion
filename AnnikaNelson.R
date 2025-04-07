install.packages("gitcreds")
library(gitcreds)
gitcreds_set()
`

library(ggplot2)
project <- read.csv("C:/Users/annik/OneDrive - Tulane University/Desktop/Vibrio/Vibrio Project R.csv")
ggplot (enumeration_plot, aes(x=Date_Collected, y=Enumeration, group=Name, color=Name)) +
  geom_point() +
  geom_line() +
  labs(
    x="Date Collected",
    y="*Vibrio* spp. (CFU/mL)",
    color="Site Name") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  facet_wrap(~enumeration_plot$Name)+
  theme(axis.text.x = element_text(angle =60, hjust = 1, size=7), axis.title.y = ggtext :: element_markdown(), legend.text = ggtext :: element_markdown())
`
