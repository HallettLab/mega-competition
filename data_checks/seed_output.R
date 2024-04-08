library(tidyverse)

model.dat <- read.csv("data/model_dat.csv")



model.sum <- model.dat %>%
  group_by()


ggplot(model.dat[!model.dat$phyto %in% c("AVBA", "CLPU"),], aes(x=phyto.seed.out, color = treatment)) + 
  geom_density(linewidth=1.5) +
  facet_wrap(~phyto, scales = "free") +
  scale_color_manual(values = c("#70a494", "#de8a5a")) +
  xlab("Seeds Out")

ggsave("data_checks/preliminary_figures/seeds_out.png", width = 10, height = 8)
