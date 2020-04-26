library(jsonlite)
library(tidyverse)

MechaCar_mpg<-read.csv('MechaCar_mpg.csv',check.names = T, stringsAsFactors = F)
head(MechaCar_mpg)
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_mpg))

MechaCar_suscoil<-read.csv('Suspension_Coil.csv',check.names = T, stringsAsFactors = F)
head(MechaCar_suscoil)
summary <- MechaCar_suscoil %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),Standard_Deviation_PSI=sd(PSI))
summary

Lot1 <- MechaCar_suscoil %>% filter(MechaCar_suscoil$Manufacturing_Lot == 'Lot1')
Lot2 <- MechaCar_suscoil %>% filter(MechaCar_suscoil$Manufacturing_Lot == 'Lot2')
Lot3 <- MechaCar_suscoil %>% filter(MechaCar_suscoil$Manufacturing_Lot == 'Lot3')

t.test(Lot1$PSI,mu = 1500)
t.test(Lot2$PSI,mu = 1500)
t.test(Lot3$PSI,mu = 1500)

sample_sus <- MechaCar_suscoil %>% sample_n(50)
sample_sus2 <- MechaCar_suscoil %>% sample_n(50)
t.test(sample_sus$PSI,mu=1500)

t.test(Lot1$PSI, Lot2$PSI, paired = T)
t.test(Lot1$PSI, Lot3$PSI, paired = T)
t.test(Lot2$PSI, Lot3$PSI, paired = T)

mtcars <- mtcars
?mtcars
head(mtcars)
mtcars_filt <- mtcars[,c("mpg", "cyl", "hp","wt", "gear", "carb")]
mtcars_filt$cyl <- factor(mtcars_filt$cyl)
mtcars_filt$gear <- factor(mtcars_filt$gear)
mtcars_filt$carb <- factor(mtcars_filt$carb)
summary(lm(mpg~hp+wt,data=mtcars_filt))

summary(aov(mpg ~ cyl,data=mtcars_filt))
summary(aov(mpg ~ gear,data=mtcars_filt))
summary(aov(mpg ~ carb,data=mtcars_filt))

