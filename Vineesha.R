
library(dbplyr)
a <- sum(2,3)
print(a)
install.packages("gitcreds")

library(dbplyr)
a <- sum(2,3)
print(a)
install.packages("gitcreds")

accretion <- read.csv("dat.csv", stringsAsFactors = TRUE)
plot(accretion$Shannon,accretion$Accretion)
plot(accretion$Simpson,accretion$Accretion)
plot(accretion$InvSimpson,accretion$Accretion)