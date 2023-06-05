source("data_cleaning/format_model_dat.R")

## check models with high correlation 
## ACAM, AMME, BRNI, MAEL
source("data_cleaning/phyto-processing_data-cleaning/ACAM_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/AMME_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/BRNI_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/MAEL_phyto.R")
source("data_cleaning/unique_key.R")

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
## distributions ####
### all backgrounds ####
ggplot(all.phytos.info[!all.phytos.info$phyto %in% c("AVBA", "CLPU"),], aes(x=phyto.seed.out, color = treatment)) +
  geom_density() +
  facet_wrap(~phyto, scales = "free") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  xlab("Seeds Out, All Backgrounds")
ggsave("models/CW/preliminary_figures/sanity_checks/seeds_out_by_trt_distrib.png", width = 10, height = 8)

### controls only ####
ggplot(all.phytos.info[!all.phytos.info$phyto %in% c("AVBA", "CLPU") & all.phytos.info$bkgrd == "Control",], aes(x=phyto.seed.out, color = treatment)) +
  geom_density() +
  facet_wrap(~phyto, scales = "free") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  xlab("Seeds Out, Control Only")
ggsave("models/CW/preliminary_figures/sanity_checks/seeds_out_by_trt_distrib_controls_only.png", width = 10, height = 8)

ggplot(all.phytos.info[!all.phytos.info$phyto %in% c("AVBA", "CLPU") & all.phytos.info$bkgrd == "Control",], aes(x=phyto.seed.out, fill = treatment)) +
  geom_histogram() +
  facet_wrap(~phyto*treatment, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  xlab("Seeds Out, Controls Only")

ggsave("models/CW/preliminary_figures/sanity_checks/seeds_out_by_trt_controls_only_histo.png", width = 10, height = 8)

## boxplots ####
ggplot(all.phytos.info[all.phytos.info$phyto != "AVBA" & all.phytos.info$phyto != "CLPU",], aes(x=treatment, y=phyto.seed.out, color = treatment)) +
  geom_boxplot() +
  facet_wrap(~phyto, scales = "free", ncol = 4, nrow = 4) +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ylab("Phytometer Seeds Out") + xlab("Treatment")

ggsave("models/CW/preliminary_figures/sanity_checks/seeds_out_by_trt.png", width = 10, height = 8)

## mean across backgrounds ####
all.out <- all.phytos.info %>%
  group_by(treatment, phyto) %>%
  mutate(mean.seeds.out = mean(phyto.seed.out), se.seeds.out = calcSE(phyto.seed.out))

ggplot(all.out, aes(x=treatment, y=mean.seeds.out, color = treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean.seeds.out-se.seeds.out, ymax=mean.seeds.out+se.seeds.out)) +
  facet_wrap(~phyto, scales = "free") +
  scale_color_manual(values = c("#003366", "#FFA630"))


# Plot Lambda Priors ####
ggplot(lambda_priors_mean[!lambda_priors_mean$phyto %in% c("AVBA", "CLPU"),], aes(x=treatment, y=mean_seeds_ctrl, color = treatment)) +
  geom_point(size = 3) +
  facet_wrap(~phyto, scales = "free") +
  geom_errorbar(aes(ymin = mean_seeds_ctrl-sd_seeds, ymax = mean_seeds_ctrl + sd_seeds)) +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Mean Seeds Control w/1 sd above & below") + xlab("Treatment") +
  ggtitle("Lambda Priors (Mean)")

ggsave("models/CW/preliminary_figures/sanity_checks/lambda_priors_mean.png", width = 10, height = 8)

ggplot(lambda_priors_max[!lambda_priors_max$phyto %in% c("AVBA", "CLPU"),], aes(x=treatment, y=max_seeds_ctrl, color = treatment)) +
  geom_point(size = 3) +
  facet_wrap(~phyto, scales = "free") +
  geom_errorbar(aes(ymin = max_seeds_ctrl-sd_seeds, ymax = max_seeds_ctrl + sd_seeds)) +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Max Seeds Control w/1 sd above & below") + xlab("Treatment") +
  ggtitle("Lambda Priors (Max)")

ggsave("models/CW/preliminary_figures/sanity_checks/lambda_priors_max.png", width = 10, height = 8)


# Check Correlated Models ####
## ACAM D ####
ggplot(acam_final, aes(x=total.biomass.g.rounded)) +
  geom_histogram()

ggplot(acam.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()
ggplot(acam.phyto, aes(x=phyto.seed.out)) +
  geom_density()

acam.low <- left_join(acam.phyto, unique.key, by = c("unique.ID", "phyto")) %>%
  filter(treatment == "D")

## MAEL D ####
ggplot(mael_final, aes(x=flower.num, color=treatment)) +
  geom_density() +
  facet_wrap(~treatment) +
  ggtitle("MAEL") +
  xlab("Flower Number") +
  scale_color_manual(values = c("#003366", "#FFA630"))

ggplot(mael_final, aes(y=flower.num, x=treatment, color = treatment)) +
  geom_boxplot() +
  facet_wrap(~treatment, scales = "free") +
  ggtitle("MAEL") +
  xlab("Flower Number") +
  scale_color_manual(values = c("#003366", "#FFA630"))

ggplot(mael_final[mael_final$treatment == "D",], aes(x=flower.num)) +
  geom_histogram(bins = 200) +
  #facet_wrap(~treatment) +
  ggtitle("MAEL D") +
  xlab("Flower Number") #+
  scale_fill_manual(values = c("#003366", "#FFA630"))

mael_zeros <- mael_final %>%
  filter(flower.num == 0)
## 22 values were 0

mael_zeros2 <- mael.phyto %>%
  filter(phyto.seed.out == 0)





