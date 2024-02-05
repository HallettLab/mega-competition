
library(tidyverse)

## replicate information
reps <- read.csv("models/CW/replicate-info.csv")

lowreps <- reps %>% 
  filter(true.reps < 4)
