
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

library(nlme)

m1 <- gls(Accretion ~ Shannon*CommunityStationFront , data = accretion) 
summary(m1)
anova(m1,type="marginal") #Type III anova


library(nlme)

options(contrasts = c("contr.sum", "contr.poly"))

accretion$CommunityStationFront <- factor(accretion$CommunityStationFront)

contrasts(accretion$CommunityStationFront)

# Fit the model
m1 <- gls(sqrt(Accretion) ~ Shannon * CommunityStationFront, data = accretion)

# Summary of the model
summary(m1)

# Type III (marginal) ANOVA
anova(m1, type = "marginal")


m1resid<-resid(m1, type="pearson")
hist(m1resid)

plot(fitted(m1),m1resid) 
abline(0,0)

plot(y = m1resid, x = accretion$Shannon, xlab = "Shannon", ylab = "Residuals")
abline(0,0)

