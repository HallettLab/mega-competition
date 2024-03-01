# Set up env
library(tidyverse)
theme_set(theme_bw())

## read in model data
model.dat <- read.csv("data/model_dat.csv") %>%
  filter(!phyto %in% c("AVBA", "CLPU"))

## visualize num phytos / block (not sep by background sp)
ggplot(model.dat, aes(x=phyto)) +
  geom_bar() +
  facet_wrap(~block, ncol = 3, nrow = 4) +
  theme(axis.text.x=element_text(angle=45, hjust = 1))

ggsave("data_checks/preliminary_figures/num_phyto_by_block.png", width = 8, height = 6)

## visualize num phytos / background / block
## loop thru each sp to create separate fig
species <- c("ACAM", "ANAR", "AMME", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

for (i in 1:length(species)) {
  
  ## filter out sp
  dat <- model.dat %>%
    filter(phyto == species[i])
  
  ## visualize
  ggplot(dat, aes(x=bkgrd)) +
    geom_bar() +
    facet_wrap(~block) +
    theme(axis.text.x=element_text(angle=90, hjust = 1)) +
    ggtitle(paste0(species[i], "_phyto")) 
  
  ## save
  ggsave(paste0("data_checks/preliminary_figures/", species[i], "_num_phyto_by_block_by_bgsp.png"), width = 10, height = 6)
  
}
