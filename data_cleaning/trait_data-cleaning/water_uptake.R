## Set up Env
library(tidyverse)
library(openxlsx)

# read in data ####
## file path
lead <- "/Users/carme/Dropbox (University of Oregon)/Greenhouse_Traits/Data/"


compost_caitlin_WU <- read.xlsx(paste0(lead, "Entered_Data/water_uptake/greenhouse_water-uptake_spring2022.xlsx"), sheet = 1)
lina_WU <- read.xlsx(paste0(lead, "Entered_Data/water_uptake/greenhouse_water-uptake_spring2022.xlsx"), sheet = 2)
carmen_WU <- read.xlsx(paste0(lead, "Entered_Data/water_uptake/greenhouse_water-uptake_spring2022.xlsx"), sheet = 3)
controls_WU <- read.xlsx(paste0(lead, "Entered_Data/water_uptake/greenhouse_water-uptake_spring2022.xlsx"), sheet = 4)

# Check Col Names ####

colnames(carmen_WU) == colnames(lina_WU)
colnames(compost_caitlin_WU) == colnames(lina_WU)
colnames(compost_caitlin_WU) == colnames(controls_WU) ## Nope

controls_WU2 <- controls_WU %>%
  mutate(Project = "control",
         date.germ. = NA) %>%
  select(1:2, 10, 11, 3:9) ## reorder columns

colnames(compost_caitlin_WU) == colnames(controls_WU2)



# Fill in Species ####
head(compost_caitlin_WU)

compost_caitlin_WU2 <- compost_caitlin_WU %>% 
  fill(Code, .direction = "down")

compost_caitlin_WU$Code
compost_caitlin_WU2$Code

head(lina_WU)

lina_WU2 <- lina_WU %>% 
  fill(Code, .direction = "down")

lina_WU$Code
lina_WU2$Code


head(carmen_WU)

carmen_WU2 <- carmen_WU %>% 
  fill(Code, .direction = "down")

carmen_WU$Code
carmen_WU2$Code

# Merge ####
all_WU <- do.call("rbind", list(compost_caitlin_WU2, lina_WU2, carmen_WU2, controls_WU2))

# Fix Col Names ####
colnames(all_WU) <- c("Rep", "Code", "Project", "date.germ", "date.wet.weigh", "time.wet.weigh", "wet.weight.g", "date.dry.weigh", "time.dry.weigh", "dry.weight.g","Notes")

# Fix Date Format ####
all_WU$date.germ <- as.Date(all_WU$date.germ, origin = "1899-12-30")
all_WU$date.wet.weigh <- as.Date(all_WU$date.wet.weigh, origin = "1899-12-30")
all_WU$date.dry.weigh <- as.Date(all_WU$date.dry.weigh, origin = "1899-12-30")

# Fix Time Format ####

head(all_WU)


