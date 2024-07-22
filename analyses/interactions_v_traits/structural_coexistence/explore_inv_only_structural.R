## save the invasive only structural dat
write.csv(invcommC, "analyses/interactions_v_traits/structural_coexistence/inv_only_structural_results_D_20240713.csv")

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/"

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


## set up for visualisation
invcommC_vis = invcommC %>%
  filter(!is.na(BRHO))%>%
  mutate(comp = paste0(ANAR, BRHO, CESO, LOMU, TACA, THIR))

## Explore invasive sp structural results
ggplot(invcommC, aes(x=feasibility)) +
  geom_bar()

ggsave(paste0(fig_loc, "inv_only_D_feasibility_barchart.png"), width = 4, height = 3)

nrow(invcommC[invcommC$feasibility == 1,])
## 54 feasible comm out of total of 3000

invcommC_vis_filt = invcommC_vis %>%
  filter(is.na(feasibility))

unique(invcommC_vis_filt$comp)

## niche diffs
ggplot(invcommC, aes(x=niche_diff)) +
  geom_histogram()
ggsave(paste0(fig_loc, "inv_only_D_niche_diffs_hist.png"), width = 4, height = 3)

## fitness diffs
ggplot(invcommC, aes(x=fitness_diff)) +
  geom_histogram()
ggsave(paste0(fig_loc, "inv_only_D_fitness_diffs_hist.png"), width = 4, height = 3)

## by composition & legume presence
ggplot(invcommC_vis, aes(x=niche_diff, fill = as.factor(THIR))) +
  geom_histogram() +
  facet_wrap(~comp, ncol = 5, nrow = 3, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed")
ggsave(paste0(fig_loc, "inv_only_D_niche_diffs_hist.png"), width = 10, height = 6)

ggplot(invcommC_vis, aes(x=niche_diff, color = as.factor(comp))) +
  geom_density()   
ggsave(paste0(fig_loc, "inv_only_D_niche_diffs_densplot_comps.png"), width = 6, height = 4)
  
## calculate proportion feasible
prop_feas = invcommC %>%
  filter(niche_diff != -Inf) %>%
  mutate(comp = paste0(ANAR, BRHO, CESO, LOMU, TACA, THIR)) %>%
  group_by(comp) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(w_legume = ifelse(substr(comp, start = 6, stop = 6) == 1, 1, 0)) %>%
  filter(!is.na(num_feas))

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible, fill = as.factor(w_legume))) +
  geom_bar(stat = 'identity') +
  ggtitle("Invasive only 4sp Comm") +
  xlab("Composition") +
  ylab("Prop Feasible Comm (200 draws)")
ggsave(paste0(fig_loc, "inv_only_D_4spcomm.png"), width = 10, height = 3)

ggplot(prop_feas, aes(x=mean_niche, y=mean_fitness, color = as.factor(w_legume))) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_fitness - se_fitness, ymax = mean_fitness + se_fitness), width = 0.05)+
  geom_errorbarh(aes(xmax = mean_niche + se_niche, xmin = mean_niche - se_niche), height = 1)
ggsave(paste0(fig_loc, "inv_only_D_niche_fitness_diffs_scatter.png"), width = 6, height = 3)
