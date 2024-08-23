## models for fdiv v prop feasible, etc.

m1 = aov(prop_feasible ~ origin+treatment+as.factor(num.inv), data = allcomm_sum)
summary(m1)

TukeyHSD(m1)

m2 = lm(prop_feasible ~ fdiv+origin+treatment, data = allcomm_sum)
summary(m2)

summary(aov(m2))
## testing whether conditions are diff from each other; how to test whether slopes are diff from 0?