source("analyses/traits/trait_pcas_exploration.R")


library(bayestestR)

mean.alphas <- ricker_posteriors_long %>%
  group_by(species, alpha_name) %>%
  summarise(mean.alpha = mean(alpha_value),
            ints = hdi(alpha_value))
  

pc.scores <- MC.pca.ID %>%
  mutate(species = phyto) %>%
  select(species, PC1, PC2) %>%
  group_by(species) %>%
  summarise(mean.PC1 = mean(PC1),
            mean.PC2 = mean(PC2))


#plot(pc.scores$mean.PC1, mean.alphas$mean.alpha)


test <- left_join(mean.alphas, pc.scores, by = c("species"))

ggplot(test, aes(x=mean.PC1, y=mean.alpha, color = species)) +
  geom_point() +
  geom_hline(yintercept = 0)

ggplot(test, aes(x=mean.PC2, y=mean.alpha, color = species)) +
  geom_point() +
  geom_hline(yintercept = 0)
