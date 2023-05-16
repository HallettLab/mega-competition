source("models/CW/import_ricker_posteriors.R")


# Format Data ####
## change to long data format
ricker_posteriors_long <- ricker_posteriors2 %>%
  pivot_longer(2:24, names_to = "alpha_name", values_to = "alpha_value") %>%
  filter(!alpha_name %in% c("alpha_figa", "alpha_gamu", "alpha_hygl", "alpha_siga", "alpha_other", "alpha_crco", "alpha_erbo", "lp__"))

## calc mean lambda
ricker_lambda_mean <- ricker_posteriors_long %>%
  group_by(species, treatment) %>%
  summarise(mean_lambda = mean(lambda), sd_lambda = sd(lambda))

lambda_mean_C <- ricker_lambda_mean %>%
  filter(treatment == "C", species == "BRHO" | species == "GITR" | species == "LENI" | species == "TWIL")



## calc mean alphas
ricker_alpha_mean_C <- ricker_posteriors_long %>%
  filter(treatment == "C") %>%
  group_by(species, treatment, alpha_name) %>%
  summarise(mean_alpha = mean(alpha_value)) %>%
  pivot_wider(names_from = "alpha_name", values_from = "mean_alpha") %>%
  ungroup() %>%
  select(-treatment) %>%
  filter(species == "BRHO" | species == "GITR" | species == "LENI" | species == "TWIL") %>%
  select(alpha_brho, alpha_gitr, alpha_leni, alpha_twil)

ricker_alpha_mean_C <- as.data.frame(ricker_alpha_mean_C)
rownames(ricker_alpha_mean_C) <- ricker_alpha_mean_C[,1]
ricker_alpha_mean_C <- ricker_alpha_mean_C[,-1]

ricker_alpha_mean_D <- ricker_posteriors_long %>%
  filter(treatment == "D") %>%
  group_by(species, treatment, alpha_name) %>%
  summarise(mean_alpha = mean(alpha_value)) %>%
  pivot_wider(names_from = "alpha_name", values_from = "mean_alpha")


## Test out invasion schemes
## Control
A_C <- as.matrix(ricker_alpha_mean_C)
b_C <- lambda_mean_C$mean_lambda

IS_C <- LV.IS(A_C,b_C)
out_C <- IG.function(IS_C)

out_C$IG

plot.IG.alt(out_C)

out_C$acyclic
out_C$permanent

FindEndStates(out=out_C)

## Drought 




