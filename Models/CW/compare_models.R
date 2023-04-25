## Compare BH and Ricker

# Lambda ####
ricker_lambda_mean <- ricker_lambda_mean %>%
  mutate(model = "ricker")

BH_lambda_mean <- BH_lambda_mean %>%
  mutate(model = "BH")

all_lambda_means <- rbind(ricker_lambda_mean, BH_lambda_mean)

lambda_diffs <- all_lambda_means %>%
  group_by(species, treatment) %>%
  summarise(lambda_diff = diff(mean_lambda))

## visualize
ggplot(all_lambda_means, aes(x=model, y=mean_lambda, color = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  geom_errorbar(aes(ymin = mean_lambda - sd_lambda, ymax = mean_lambda + sd_lambda), width = 0.25) +
  facet_wrap(~species, scales = "free") +
  ggtitle("BH - Ricker Comparison, Mean Lambda") +
  ylab("Mean Lambda") +
  xlab("Model")

ggsave("models/CW/preliminary_figures/model_comparison_lambda.png", width = 14, height = 10)

ggplot(lambda_diffs, aes(x=species, y=lambda_diff, color = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  #geom_errorbar(aes(ymin = mean_lambda - sd_lambda, ymax = mean_lambda + sd_lambda), width = 0.25) +
  #facet_wrap(~species, scales = "free") +
  ggtitle("BH - Ricker Mean Lambda Differences") +
  ylab("BH Lambda - Ricker Lambda") +
  xlab(" ") +
  theme(axis.text.x = element_text(angle = 20)) +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("models/CW/preliminary_figures/model_comparison_lambda_diff.png", width = 8, height = 5)

# Alpha ####
## mean ####
ricker_alpha_mean <- ricker_alpha_mean %>%
  mutate(model = "ricker")

BH_alpha_mean <- BH_alpha_mean %>%
  mutate(model = "BH")

all_alpha_means <- rbind(ricker_alpha_mean, BH_alpha_mean)

## difference ####
alpha_diffs <- all_alpha_means %>%
  group_by(species, treatment, alpha_name, alpha) %>%
  mutate(alpha_diff = diff(mean_alpha))

## visualize ####
ggplot(alpha_diffs, aes(x=alpha, y=alpha_diff, color = treatment)) +
  geom_point() +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  #geom_errorbar(aes(ymin = mean_alpha - sd_alpha, ymax = mean_alpha + sd_alpha)) +
  facet_wrap(~species, ncol = 3, nrow = 6) +
  ggtitle("BH - Ricker Alpha Differences") +
  theme(axis.text.x = element_text(angle = 20)) +
  ylab("alpha_BH - alpha_ricker") +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("models/CW/preliminary_figures/model_comparison_alpha_diff.png", width = 14, height = 10)

ggplot(all_alpha_means, aes(x=alpha, y=mean_alpha, color = model)) +
  geom_point() +
  #scale_color_manual(values = c("#003366", "#FFA630"))+
  geom_errorbar(aes(ymin = mean_alpha - sd_alpha, ymax = mean_alpha + sd_alpha)) +
  facet_wrap(~species, ncol = 3, nrow = 6) +
  ggtitle("BH - Ricker Comparison, Mean Alphas") +
  theme(axis.text.x = element_text(angle = 20)) +
  ylab("alpha value") + xlab("alpha name") +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("models/CW/preliminary_figures/model_comparison_mean_alpha.png", width = 14, height = 10)

