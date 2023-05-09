source("data_cleaning/format_model_dat.R")

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
