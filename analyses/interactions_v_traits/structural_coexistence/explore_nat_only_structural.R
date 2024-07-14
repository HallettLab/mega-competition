write.csv(natcommC_vis, "analyses/interactions_v_traits/structural_coexistence/nat_only_structural_results_20240713.csv")


fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/"

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


## set up for visualisation
natcommC_vis = allcommC %>%
  filter(!is.na(GITR))%>%
  mutate(comp = paste0(GITR, LENI, MICA, PLER, ACAM, AMME, MAEL, PLNO, TWIL))

## Explore native sp structural results
ggplot(allcommC, aes(x=feasibility)) +
  geom_bar()
## 22601 NAs for feasibility, N diff, and Fitness diff

ggsave(paste0(fig_loc, "nat_sp_feasibility_barchart.png"), width = 4, height = 3)

nrow(natcommC_vis[natcommC_vis$feasibility == 1 & !is.na(natcommC_vis$feasibility),])
## 34 feasible comm 
nrow(natcommC_vis[!is.na(natcommC_vis$feasibility),])
## 2000 rows that are not NAs for feasibility, etc. 

nat_comms_filt = natcommC_vis %>%
  filter(!is.na(feasibility))
## all sp present in these comms

unique(nat_comms_filt$comp)
## 10 unique communities; 10x200 = 2000; calculations for these communities worked every time & the ones that did not work didn't work any of the 200 times

## comm with NAs
nat_comms_NA = natcommC_vis %>%
  filter(is.na(feasibility))





## niche diffs
ggplot(natcommC_vis, aes(x=niche_diff)) +
  geom_histogram()
ggsave(paste0(fig_loc, "nat_only_D_niche_diffs_hist.png"), width = 4, height = 3)

## fitness diffs
ggplot(natcommC_vis, aes(x=fitness_diff)) +
  geom_histogram()
ggsave(paste0(fig_loc, "nat_only_D_fitness_diffs_hist.png"), width = 4, height = 3)

## by composition & legume presence
ggplot(natcommC_vis, aes(x=niche_diff, fill = as.factor(ACAM))) +
  geom_histogram() +
  facet_wrap(~comp, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed")
#ggsave(paste0(fig_loc, "niche_diffs_hist.png"), width = 10, height = 6)

ggplot(natcommC_vis, aes(x=niche_diff, color = as.factor(comp))) +
  geom_density()   
ggsave(paste0(fig_loc, "nat_only_D_niche_diffs_densplot_comps.png"), width = 6, height = 4)

## calculate proportion feasible
prop_feas = natcommC_vis %>%
  #filter(niche_diff != -Inf) %>%
 # mutate(comp = paste0(ANAR, BRHO, CESO, LOMU, TACA, THIR)) %>%
  group_by(comp) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(w_legume = ifelse(substr(comp, start = 5, stop = 5) == 1, 1, 
                           ifelse(substr(comp, start = 9, stop = 9) == 1, 1, 0))) %>%
  filter(!is.na(num_feas))

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible, fill = as.factor(w_legume))) +
  geom_bar(stat = 'identity') +
  ggtitle("Invasive only 4sp Comm") +
  xlab("Composition") +
  ylab("Prop Feasible Comm (200 draws)")
ggsave(paste0(fig_loc, "nat_only_D_4spcomm.png"), width = 10, height = 3)

ggplot(prop_feas, aes(x=mean_niche, y=mean_fitness, color = as.factor(w_legume))) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_fitness - se_fitness, ymax = mean_fitness + se_fitness), width = 0.05)+
  geom_errorbarh(aes(xmax = mean_niche + se_niche, xmin = mean_niche - se_niche), height = 1)
ggsave(paste0(fig_loc, "nat_only_D_niche_fitness_diffs_scatter.png"), width = 6, height = 3)
