nonnative <- c("ANAR", "BRHO", "BRNI", "CESO", "LOMU", "TACA", "THIR")
grass <- c("BRHO", "TACA", "LOMU")
legume <- c("ACAM", "THIR", "TWIL")

inter <- alpha_sums_89hdi.filt %>%
  filter(species != toupper(substr(alpha_name, 7, 10)))

ggplot(inter, aes(x = interaction_type)) +
  geom_bar() +
  xlab("Interaction Type") +
  ggtitle("INTERspecific interaction types")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/inter_interaction_type_summary.png", width = 3, height = 2)


inter_posteriors <- interxn %>%
  filter(species != toupper(substr(alpha_name, 7, 10)))

inter_all <- left_join(inter_posteriors, inter, by = c("species", "alpha_name", "combo")) %>%
  group_by(species, alpha_name, combo) %>%
  filter(alpha_value > ci_lo & alpha_value < ci_hi) %>%
  mutate(treatment = ifelse(substr(alpha_name, 12, 13) == "d", "D", "C"),
         alpha_name = substr(alpha_name, 1, 10))

inter_sum <- inter_all %>%
  group_by(species, alpha_name, treatment) %>%
  summarise(median_alpha = median(alpha_value)) %>%
  mutate(origin_sp = ifelse(species %in% nonnative, "non-native", "native"),
         funct_group_sp = ifelse(species %in% grass, "grass",
                              ifelse(species %in% legume, "legume", "forb")),
         fg_origin_sp = paste(origin_sp, funct_group_sp, sep = "_"),
         origin_al = ifelse(toupper(substr(alpha_name, 7, 10)) %in% nonnative, "non-native", "native"),
         funct_group_al = ifelse(toupper(substr(alpha_name, 7, 10)) %in% grass, "grass",
                                 ifelse(toupper(substr(alpha_name, 7, 10)) %in% legume, "legume", "forb")),
         fg_origin_al = paste(origin_al, funct_group_al, sep = "_"))


ggplot(inter_sum, aes(x=alpha_name, y=median_alpha, color = fg_origin_al)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E"))+
  xlab("") +
  ylab("Median Alpha") +
  labs(color = "Functional Group") +
  theme(axis.text.x=element_text(angle=45, hjust = 1))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/inter_sp_median_alphas_by_alpha.png", width = 6, height = 4)

ggsave("analyses/explore_interaction_coeff/preliminary_figures/inter_sp_median_alphas_by_alpha_boxplot.png", width = 6, height = 4)


ggplot(inter_sum, aes(x=species, y=median_alpha, color = fg_origin_sp)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E"))+
  xlab("") +
  ylab("Median Alpha") +
  labs(color = "Functional Group") +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) +
  ggtitle("Interactions experienced by species")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/inter_sp_median_alphas_by_sp_fgcolor.png", width = 6, height = 4)











