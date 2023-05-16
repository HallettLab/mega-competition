source("data_cleaning/format_model_dat.R")

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Control Seeds ####
## check that lambda values and seed output in controls align somewhat for species (i.e. if there is a higher lambda in the drought vs. control is this mirrored in the seed output by treatment data?)

ggplot(ctrl_seed_output_check, aes(x=treatment, y=mean.seeds, color = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  geom_errorbar(aes(ymin=mean.seeds-sd.seeds, ymax = mean.seeds+sd.seeds)) +
  facet_wrap(~phyto, scales = "free", ncol = 3, nrow = 6) +
  xlab("Treatment") + 
  ylab("Mean Seed Output") +
  ggtitle("Seed output in control backgrounds")
ggsave("models/CW/preliminary_figures/sanity_checks/control_seeds_out_by_trt.png", width = 8, height = 9)

# All Seeds Out ####
ggplot(all.phytos.info, aes(x=phyto.seed.out, color = treatment)) +
  geom_density() +
  facet_wrap(~phyto, scales = "free") +
  scale_color_manual(values = c("#003366", "#FFA630"))

ggplot(all.phytos.info, aes(x= treatment, y=phyto.seed.out)) +
  geom_boxplot() +
  facet_wrap(~phyto, scales = "free")


all.out <- all.phytos.info %>%
  group_by(treatment, phyto) %>%
  mutate(mean.seeds.out = mean(phyto.seed.out), se.seeds.out = calcSE(phyto.seed.out))

ggplot(all.out, aes(x=treatment, y=mean.seeds.out, color = treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean.seeds.out-se.seeds.out, ymax=mean.seeds.out+se.seeds.out)) +
  facet_wrap(~phyto, scales = "free") +
  scale_color_manual(values = c("#003366", "#FFA630"))
