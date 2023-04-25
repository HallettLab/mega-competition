source("models/CW/import_BH_posteriors.R")
reps <- read.csv("models/CW/replicate-info.csv")
theme_set(theme_bw())

# Format Data ####
## change to long data format
BH_posteriors_long <- BH_posteriors2 %>%
  pivot_longer(2:25, names_to = "alpha_name", values_to = "alpha_value") %>%
  filter(!alpha_name %in% c("alpha_figa", "alpha_gamu", "alpha_hygl", "alpha_siga", "alpha_other", "lp__"))

## calc lambda mean
BH_lambda_mean <- BH_posteriors_long %>%
  group_by(species, treatment) %>%
  summarise(mean_lambda = mean(lambda), sd_lambda = sd(lambda))

## calc mean alphas
## filter to remove combos without enough replicates
good.reps <- reps %>%
  filter(true.reps > 3)

good.reps.vec <- unique(good.reps$combos)

BH_alpha_mean <- BH_posteriors_long %>%
  mutate(combos = paste(species, toupper(substr(alpha_name, 7, 11)), treatment, sep = "_")) %>%
  filter(combos %in% good.reps.vec) %>%
  group_by(species, treatment, alpha_name) %>%
  summarise(mean_alpha = mean(alpha_value), sd_alpha = sd(alpha_value)) %>%
  mutate(alpha = substr(alpha_name, 7, 11))

# Save figures ####
## Alphas ####
sp <- c("PLER", "BRHO", "GITR", "AVBA", "ANAR",  "TACA", "LOMU", "TWIL", "THIR", "CESO", "MICA", "AMME", "PLNO", "ACAM", "BRNI", "LENI", "CLPU", "MAEL")

## save model coeff
for(i in sp){
  
  alpha <- ggplot(BH_posteriors_long[BH_posteriors_long$alpha_name == paste0("alpha_", tolower(i)),], aes(x = alpha_value, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~species, ncol = 3, scales = "free")+
    scale_fill_manual(values = c("#003366", "#FFA630")) +
    ggtitle("BH Model") + xlab(paste0("alpha_", tolower(i))) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(alpha, file=paste0("models/CW/preliminary_figures/BH_model_posteriors/", "alpha_", tolower(i), "_BH.png"), width = 12, height = 10)
  
  invader <- ggplot(BH_posteriors_long[BH_posteriors_long$species == i,], aes(x = alpha_value, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~alpha_name, ncol = 3, scales = "free") +
    scale_fill_manual(values = c("#003366", "#FFA630")) +
    ggtitle(paste0(i, " invader BH")) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(invader, file=paste0("models/CW/preliminary_figures/BH_model_posteriors/", i, "_inter_alphas_BH.png"), width = 14, height = 10)
  
}

## Lambda ####
ggplot(BH_posteriors2, aes(x = lambda, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630"))+
  ggtitle("BH")
ggsave(file=paste0("models/CW/preliminary_figures/BH_model_posteriors/lambda_BH.png"), width = 14, height = 10)

## Traceplots ####
sp_trt <- names(BH_plots)

pdf("models/CW/preliminary_figures/BH_model_posteriors/BH_model_trace_plots.pdf", width = 12, height = 8)

for(i in 1:length(sp_trt)){
  
  ## traceplots are saved in BH_plots list
  print(BH_plots[[i]] + ggtitle(sp_trt[i]))
  
}

dev.off()

## Means ####

ggplot(BH_lambda_mean, aes(x=treatment, y=mean_lambda, color = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  geom_errorbar(aes(ymin = mean_lambda - sd_lambda, ymax = mean_lambda + sd_lambda), width = 0.25) +
  facet_wrap(~species, scales = "free")


ggplot(BH_alpha_mean, aes(x=alpha, y=mean_alpha, color = treatment)) +
  geom_point() +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  geom_errorbar(aes(ymin = mean_alpha - sd_alpha, ymax = mean_alpha + sd_alpha)) +
  facet_wrap(~species, ncol = 3, nrow = 6, scales = "free") +
  ggtitle("Mean Alpha, BH") +
  theme(axis.text.x = element_text(angle = 20))
