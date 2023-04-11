# inspect dens plots
# species having a hard time reaching equilibrium
## dry: TWIL, CESO, PLNO
## wet: TWIL, THIR, AMME, ACAM
ggplot(posteriors2, aes(x = lambda, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630"))

#ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/lambda_ricker.png", width = 8, height = 7)


ggplot(posteriors2, aes(x = alpha_pler, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")+
  scale_fill_manual(values = c("#003366", "#FFA630"))

ggplot(posteriors2, aes(x = alpha_brho, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")+
  scale_fill_manual(values = c("#003366", "#FFA630"))

ggplot(posteriors2, aes(x = alpha_leni, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_brni, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_gitr, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_acam, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_avba, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")
## I don't trust alpha_avba until we've accounted for backgrounds that barely had any background individuals somehow.

ggplot(posteriors2, aes(x = alpha_anar, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_mael, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_clpu, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_taca, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_lomu, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_twil, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_thir, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_ceso, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_mica, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_amme, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_plno, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

rm(params, tmp, tmp2, tmp3, i, j)

# check scales of lambdas and alphas ####
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

posteriors2$scaled <- posteriors2$lambda/(1+rowSums(posteriors2[2:19]))

posteriors2.sum <- posteriors2 %>%
  group_by(species, treatment) %>%
  summarize(lambda.scaled.mean = mean(scaled),
            lambda.scaled.se = calcSE(scaled))


ggplot(posteriors2.sum, aes(x=species, y=lambda.scaled.mean, color = treatment)) +
  geom_point()


# Species by Species params ####



ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/lambda_all_sp_weeds.png", width = 8, height = 7)

## PLER
ggplot(posteriors_long[posteriors_long$species == "PLER",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("PLER invader")

#ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/PLER_inter_alphas.png", width = 8, height = 7)

## BRHO
ggplot(posteriors_long[posteriors_long$species == "BRHO",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("BRHO invader")

#ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/BRHO_inter_alphas.png", width = 8, height = 7)


## GITR
ggplot(posteriors_long[posteriors_long$species == "GITR",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("GITR invader")

#ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/GITR_inter_alphas.png", width = 8, height = 7)

## AVBA 
ggplot(posteriors_long[posteriors_long$species == "AVBA",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("AVBA invader")

#ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/AVBA_inter_alphas.png", width = 8, height = 7)

## ANAR
ggplot(posteriors_long[posteriors_long$species == "ANAR",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("ANAR invader")

ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/ANAR_inter_alphas.png", width = 8, height = 7)

## MAEL
ggplot(posteriors_long[posteriors_long$species == "MAEL",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("MAEL invader")

#ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/MAEL_inter_alphas.png", width = 8, height = 7)

## TACA 
ggplot(posteriors_long[posteriors_long$species == "TACA",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("TACA invader")

ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/TACA_inter_alphas.png", width = 8, height = 7)


## LOMU
ggplot(posteriors_long[posteriors_long$species == "LOMU",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("LOMU invader")

ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/LOMU_inter_alphas.png", width = 8, height = 7)


## TWIL
ggplot(posteriors_long[posteriors_long$species == "TWIL",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("TWIL invader")

#ggplot(posteriors_long_old[posteriors_long_old$species == "TWIL",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
# geom_density() + 
# facet_wrap(~alpha_name, ncol = 3, scales = "free") +
# scale_fill_manual(values = c("#003366", "#FFA630")) +
# ggtitle("TWIL invader")

ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/TWIL_inter_alphas_old_posteriors.png", width = 8, height = 7)

## THIR
ggplot(posteriors_long[posteriors_long$species == "THIR",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("THIR invader")

ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/THIR_inter_alphas.png", width = 8, height = 7)

## CESO
ggplot(posteriors_long[posteriors_long$species == "CESO",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("CESO invader")

ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/CESO_inter_alphas.png", width = 8, height = 7)

## MICA
ggplot(posteriors_long[posteriors_long$species == "MICA",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("MICA invader")

ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/MICA_inter_alphas.png", width = 8, height = 7)

## AMME
ggplot(posteriors_long[posteriors_long$species == "AMME",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("AMME invader")

ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/AMME_inter_alphas.png", width = 8, height = 7)

## PLNO
ggplot(posteriors_long[posteriors_long$species == "PLNO",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("PLNO invader")

ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/PLNO_inter_alphas.png", width = 8, height = 7)

## ACAM 
ggplot(posteriors_long[posteriors_long$species == "ACAM",], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~alpha_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("ACAM invader")

ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/ACAM_inter_alphas.png", width = 8, height = 7)
