library(tidyverse)
library(scico)

brhodat <- read.csv("brho-allometry.csv")

## clean data frame
brho_dat <- brhodat %>%
  select(1:17)

colnames(brho_dat) <- c("block", "plot", "sub", "species", "rep", "floret.num",  "total.biomass.mg", "biomass.no.seeds.mg", "seeds.mg", "inflorescence.weight.g",  "seeds.num", "unique", "notes", "seeds.per.floret", "date", "initials", "scale.used")

brho_dat$block <- as.factor(brho_dat$block)

## visualize
theme_set(theme_bw())

ggplot(brho_dat, aes(x=inflorescence.weight.g, y=seeds.num)) +
  #geom_point() +
  geom_smooth(method = lm, color = "black", size = 0.75) +
  geom_point(aes(fill=block), 
             colour="black",pch=21, size=3) + 
  scale_fill_scico_d(palette = "roma", direction = 1) +
  xlab("Inflorescence Weight (g)") + ylab("Number of Seeds") +
  annotate(geom = "text", x=0.05, y=120, label = "R2 = 0.8915", color = "black", size = 3)

ggsave("brho-seeds-inflorescence.png", width = 5, height = 3)

seeds_inflo <- lm(seeds.num~inflorescence.weight.g, data = brho_dat)
summary(seeds_inflo)

ggplot(brho_dat, aes(x=total.biomass.mg, y=seeds.num)) +
    #geom_point() +
  geom_smooth(method = lm, color = "black", size = 0.75) +
  geom_point(aes(fill=block), 
              colour="black",pch=21, size=3) + 
  scale_fill_scico_d(palette = "roma", direction = 1) +
  xlab("Total Biomass (g)") + ylab("Number of Seeds") +
  annotate(geom = "text", x=0.1, y=120, label = "R2 = 0.4782", color = "black", size = 3)

ggsave("brho-seeds-totbio.png", width = 5, height = 3)


seeds_bio <- lm(seeds.num~total.biomass.mg, data = brho_dat)
summary(seeds_bio)
