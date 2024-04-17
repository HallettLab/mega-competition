
library(fundiversity)

## calculate trait dissimilarities

#data(package = "fundiversity")

#data("traits_birds", package = "fundiversity")

trait_sums <- MC.pca.ID %>%
  mutate(species = phyto) %>%
  group_by(species) %>%
  ## calculate mean trait values
  summarise(mean.height = mean(Height), 
            mean.LDMC = mean(LDMC),
            mean.SLA = mean(SLA), 
            mean.RMF = mean(RMF), 
            mean.CRSL = mean(CRSL), 
            mean.D = mean(D),
            mean.PF = mean(PF))

## change data to matrix format
trait.matrix <- as.matrix(trait_sums[,-1])

rownames(trait.matrix) <- unique(trait_sums$species) ## set rownames

## scale trait data
trait.matrix.sc <- scale(trait.matrix)


fd_raoq(trait.matrix.sc[1:2,])
fd_raoq(trait.matrix.sc[2:3,])

## calculate the dissimilarity matrix b/w species
dissim <- dist(trait.matrix.sc)

temp.dissim <- as.matrix(dissim)

dissim.matrix <- as.data.frame(temp.dissim)
dissim.matrix$species <- row.names(dissim.matrix)

## reformat
dissim.long <- dissim.matrix %>%
  pivot_longer(cols = c(1:16), names_to = "species2", values_to = "dissimilarity") %>%
  mutate(combo = paste(species, species2, sep = "_")) %>%
  select(combo, dissimilarity)

psums_clean <- psums %>%
  filter(parameter_type != "lambda") %>% ## remove lambda
  mutate(phyto = species,
         resident = toupper(substr(parameter_type, start = 7, stop = 10)),
         combo = paste(phyto, resident, sep = "_")) %>%
  select(combo, phyto, resident, treatment, median_parameter, parameter_type)

dissim.params <- left_join(psums_clean, dissim.long, by = c("combo"))

ggplot(dissim.params, aes(x=dissimilarity, y=median_parameter)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  ylab("Alpha value") + xlab("Trait Dissimilarity") +
  facet_wrap(~treatment)
ggsave("analyses/explore_interaction_coeff/preliminary_figures/dissim_alpha_trt.png", width = 6, height = 3.5)

ggplot(dissim.params, aes(x=dissimilarity, y=median_parameter)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  facet_wrap(~resident, scales = "free") +
  ylab("Alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Resident' Species")
ggsave("analyses/explore_interaction_coeff/preliminary_figures/dissim_alpha_residentfacet.png", width = 7, height = 6)

ggplot(dissim.params, aes(x=dissimilarity, y=median_parameter)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  facet_wrap(~phyto, scales = "free") +
  ylab("Alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Invader' Species")
ggsave("analyses/explore_interaction_coeff/preliminary_figures/dissim_alpha_invaderfacet.png", width = 7, height = 6)
