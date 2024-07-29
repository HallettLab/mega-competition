library(bayestestR)
library(tidyverse)

allcommC = read.csv("structural_6sp_66percdone_20240627.csv")

## visualize
ggplot(allcommC, aes(x=feasibility)) +
  geom_bar()
## 979794 rows with non-finite values - that's most (91%) with NAs for feasibility domain; why??

ggplot(allcommC, aes(x=niche_diff))+
  geom_histogram()
## 979828 NAs

ggplot(allcommC, aes(x=fitness_diff))+
  geom_histogram()
## 979794 NAs


feas = allcommC %>%
  filter(!is.na(feasibility))

uni_comm = feas %>%
  #group_by(ACAM, AMME, ANAR, BRHO) %>%
  select(2:15) %>%
  distinct()


inv_only = allcommC %>%
  filter(ACAM == 0, AMME == 0, GITR == 0, MAEL == 0, MICA == 0, PLER == 0, PLNO == 0, TWIL == 0) %>%
  select(ANAR, BRHO, CESO, LOMU, TACA, THIR, feasibility, niche_diff, fitness_diff, draw) %>%
  group_by(ANAR, BRHO, CESO, LOMU, TACA, THIR) %>%
  summarise(#niche_hdi_lo = hdi(niche_diff, ci = 0.95)[2],
            #niche_hdi_hi = hdi(niche_diff, ci = 0.95)[3],
            #fitness_hdi_lo = ,
            #fitness_hdi_hi = ,
            prop_feasible = sum(feasibility, na.rm = TRUE)/n())

na.check = inv_only %>%
  filter(is.na(feasibility))

na.check2 = inv_only %>%
  filter(is.na(niche_diff))

## there are ~4000 NA's... these shouldn't be resulting from bad replication, that's not as much of an issue with invasive only species. 

hdi(inv_only$niche_diff, ci = 0.95)[3]
