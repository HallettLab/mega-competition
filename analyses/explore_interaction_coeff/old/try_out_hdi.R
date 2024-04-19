
install.packages("bayestestR")
library(bayestestR)


hdi95 <- hdi(PrelimFit, ci = c(0.95))


test <- hdi(ricker_posteriors2$alpha_acam_base, ci = c(0.95))

range(ricker_posteriors2$alpha_acam_base)


testdf <- ricker_posteriors2 %>%
  group_by(species) %>%
  summarise(ints = hdi(alpha_amme_base))

ggplot(ricker_posteriors2[ricker_posteriors2$species == "ACAM",], aes(x=alpha_amme_base)) +
  geom_histogram() +
  geom_vline(xintercept = testdf[testdf$species == "ACAM",]$ints$CI_low) +
  geom_vline(xintercept = testdf[testdf$species == "ACAM",]$ints$CI_high)

test <- hdi(ricker_posteriors2$alpha_twil_base, ci = c(0.95))
