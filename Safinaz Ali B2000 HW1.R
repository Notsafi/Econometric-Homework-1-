#Safinaz Ali
#09/01/22
#B2000 Homework 1 

#1 Names of team: John Robison & Suguru Iwashiro
#2 Dice Experiment: (2, 3, 1, 4, 6, 2, 6, 1, 5, 3, 3, 6, 5, 6, 5, 6, 1, 6, 1, 5)

#4 Replicate commands from R Basic Lecture
x <- 1:50
w <- 1 + sqrt(x)/2
example1 <- data.frame(x=x, y= x + rnorm(x)*w)
attach(example1)

fm <- lm(y ~ x)
summary(fm)

lrf <- lowess(x, y)
plot(x, y)
lines(x, lrf$y)
abline(0, 1, lty=3)
abline(coef(fm))
detach()
  
load("~/Desktop/Statistic & Econmetric/Data/Household_Pulse_data.RData")
#glimpse(acs2017_ny) try this later
Household_Pulse_data[1:10,1:7]
attach(Household_Pulse_data)

summary(Household_Pulse_data)

summary(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "transgender"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "other"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "NA"])

# Average ages of men and women
mean(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "female"])

mean(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "male"])

#Lecture 1A
hist(TBIRTH_YEAR[(TBIRTH_YEAR < 1950)])
mean(TBIRTH_YEAR[ (GENID_DESCRIBE == "female") & (TBIRTH_YEAR > 1933) ]) 
summary(EEDUC)

install.packages("tidyverse")
install.packages("plyr")

library(tidyverse)
library(plyr)

summary(EST_ST)
summary(INCOME)

ddply(Household_Pulse_data, .(EST_ST), summarize, mean = round(mean(2021 - TBIRTH_YEAR), 2), sd = round(sd(2021 - TBIRTH_YEAR), 2), n_obsv = length(EST_ST))
ddply(Household_Pulse_data, .(EST_ST), summarize, age90th = quantile((2021 - TBIRTH_YEAR),probs = 0.9), age10th = quantile((2021 - TBIRTH_YEAR),probs = 0.1), n_obs = length(TBIRTH_YEAR))

table(EEDUC,GENID_DESCRIBE)
xtabs(~EEDUC + GENID_DESCRIBE)
prop.table(table(EEDUC,GENID_DESCRIBE))

mean(TBIRTH_YEAR[(REGION == "Northeast")])

restrict1 <- as.logical((REGION == "Northeast"))
dat_northeast <- subset(Household_Pulse_data, restrict1)

detach()
attach(dat_northeast)

mean(TBIRTH_YEAR)
detach()

#Something else that I learned that was interesting from the Household data is that some people with higher education was not employed in the last 7 days as people with a lower education status. 
#If you look at the data below you can see that 7,438 of people with some college are employed while only 4,118 of people with an associate degree are employed, which is unusal since they have a higher education status to get a job which tends to be a requirement for many jobs
# The reason that is surprising is because usually people with higher degrees get paid more and are more likely to get hired since they have a degree.  .  

summary(EEDUC)
summary(ANYWORK)
xtabs(~EEDUC + ANYWORK)


