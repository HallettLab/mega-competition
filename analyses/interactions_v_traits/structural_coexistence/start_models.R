
library(car)

names(sp6sum)


model.dat = sp6sum %>%
  filter(!is.na(mean_niche) ,
         mean_niche != -Inf )


m1 = aov(mean_niche ~ rainfall + as.factor(num.grass), data = model.dat)
summary(m1)

TukeyHSD(m1)


m2 = lm(mean_niche ~ cwm.sla * rainfall, data = model.dat)
summary(m2)
Anova(m2)
