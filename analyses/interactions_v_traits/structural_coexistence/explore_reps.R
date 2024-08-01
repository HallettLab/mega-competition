library(tidyverse)

reps = read.csv("data/replicate-info.csv")

bad.reps = reps %>%
  filter(true.reps < 4, 
         !phyto %in% c("AVBA", "CLPU"),
         bkgrd != "AVBA")

ggplot(bad.reps, aes(x=phyto)) +
  geom_bar() +
  facet_wrap(~treatment)

## species to remove: 
  ## in control, AMME, TWIL
bad.reps.C = bad.reps %>%
  filter(treatment == "C",
         # !phyto %in% c("PLNO"),
         !bkgrd %in% c("CESO", "ANAR", "LOMU", "BRNI", "CLPU", "TACA", "THIR", "BRHO"))

bad.reps.D = bad.reps %>%
  filter(treatment == "D",
        # !phyto %in% c("PLNO"),
         !bkgrd %in% c("CESO", "ANAR", "LOMU", "BRNI", "CLPU"))

## in drought, MAEL

ggplot(bad.reps, aes(x=phyto)) +
  geom_bar() +
  facet_wrap(~treatment)