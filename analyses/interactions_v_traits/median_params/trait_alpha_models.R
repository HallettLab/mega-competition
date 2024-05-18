# Set up ####
library(car)


## load in data
source("analyses/explore_interaction_coeff/median_params_&_traits.R")

## make new df with fg only column so that every group has at least 3 species
dat <- inter_sc %>%
  mutate(fg = ifelse(resident %in% c("TACA", "BRHO", "LOMU"), "grass",
                     ifelse(resident %in% c("TWIL", "THIR", "ACAM"), "legume", "forb")))

## test FG
m1 <- aov(alpha_scaled ~ fg, data = dat)
summary(m1)

## run models of individual traits
# LDMC ####
m_ldmc <- lm(alpha_scaled ~ mean.LDMC+fg, data = dat)
summary(m_ldmc)
Anova(m_ldmc)

## model diagnostics
qqnorm(resid(m_ldmc))
qqline(resid(m_ldmc))
## these don't look good 

plot(resid(m_ldmc) ~ fitted(m_ldmc))

# Height ####
m_height <- lm(alpha_scaled ~ mean.height*fg, data = dat)
summary(m_height)
Anova(m_height)

## model diagnostics
qqnorm(resid(m_height))
qqline(resid(m_height))
## these don't look good 

plot(resid(m_height) ~ fitted(m_height))

# *SLA ####
m_sla <- lm(alpha_scaled ~ mean.SLA*fg, data = dat)
summary(m_sla)
Anova(m_sla)

## model diagnostics
qqnorm(resid(m_sla))
qqline(resid(m_sla))
## these don't look good 

plot(resid(m_sla) ~ fitted(m_sla))

# CRSL ####
m_crsl <- lm(alpha_scaled ~ mean.CRSL*fg, data = dat)
summary(m_crsl)
Anova(m_crsl)

## model diagnostics
qqnorm(resid(m_crsl))
qqline(resid(m_crsl))
## these don't look good 

plot(resid(m_crsl) ~ fitted(m_crsl))

# FRSL ####
#m_frsl <- lm(alpha_scaled ~ mean.FRSL*fg, data = dat)
#summary(m_frsl)
#Anova(m_frsl)

## model diagnostics
#qqnorm(resid(m_frsl))
#qqline(resid(m_frsl))
## these don't look good 

#plot(resid(m_frsl) ~ fitted(m_frsl))

# RMF ####
m_rmf <- lm(alpha_scaled ~ mean.RMF*fg, data = dat)
summary(m_rmf)
Anova(m_rmf)

## model diagnostics
qqnorm(resid(m_rmf))
qqline(resid(m_rmf))
## these don't look good 

plot(resid(m_rmf) ~ fitted(m_rmf))

# D ####
m_d <- lm(alpha_scaled ~ mean.D*fg, data = dat)
summary(m_d)
Anova(m_d)

## model diagnostics
qqnorm(resid(m_d))
qqline(resid(m_d))
## these don't look good 

plot(resid(m_d) ~ fitted(m_d))

# PF ####
m_pf <- lm(alpha_scaled ~ mean.PF*fg, data = dat)
summary(m_pf)
Anova(m_pf)

## model diagnostics
qqnorm(resid(m_pf))
qqline(resid(m_pf))
## these don't look good 

plot(resid(m_pf) ~ fitted(m_pf))
