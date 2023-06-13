## Select Intra-Facil Models ####
ricker_posteriors_long <- ricker_posteriors2 %>%
  pivot_longer(2:24, names_to = "alpha_name", values_to = "alpha_value") %>%
  filter(!alpha_name %in% c("alpha_figa", "alpha_gamu", "alpha_hygl", "alpha_siga", "alpha_other", "alpha_crco", "alpha_erbo", "lp__"))

## find intra-facil models
intra_facil <- ricker_posteriors_long %>% 
  mutate(intra_facil = ifelse((species == toupper(str_sub(alpha_name, start = 7, end = 11))) & alpha_value < 0, "Y", "N")) %>%
  filter(intra_facil == "Y") %>%
  group_by(alpha_name, treatment) %>%
  summarise(intra_facil2 = unique(intra_facil)) %>%
  mutate(sp_trt = paste0(toupper(str_sub(alpha_name, start = 7, end = 11)), "_", treatment))

intra_facil2 <- unique(intra_facil$sp_trt)