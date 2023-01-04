## Set up Env
library(tidyverse)
library(openxlsx)

# read in data ####
## file path
lead <- "/Users/carme/Dropbox (University of Oregon)/Greenhouse_Traits/Data/"

## Fall ####
compost_traits_fall <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_fall2021.xlsx"), sheet = 1)

lina_traits_fall <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_fall2021.xlsx"), sheet = 2)

caitlin_traits_fall <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_fall2021.xlsx"), sheet = 3)

carmen_traits_fall <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_fall2021.xlsx"), sheet = 4)


## Spring ####
compostcaitlin_traits_spring <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_redo_spring2022.xlsx"), sheet = 1)

lina_traits_spring <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_redo_spring2022.xlsx"), sheet = 2)

carmen_traits_spring <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_redo_spring2022.xlsx"), sheet = 3)


# Subset Species ####
## Check Fall ####
### compost ####
unique(compost_traits_fall$Species)

compost_sp_overlap <- c("Amsinckia menziesii", "Bromus hordeaceus", "Centaurea solistitialis", "Crassula", "Erodium botrys", "Filago gallica", "Galium aparine", "Lolium multiflorum", "Plagiobothrys nothofulvus", "Sherardia arvensis", "Trifolium hirtum")

colnames(compost_traits_fall)

fall_comp_sub <- compost_traits_fall %>%
  filter(Species %in% compost_sp_overlap) %>%
  select(-X16)

colnames(fall_comp_sub) <- c("Rep", "Code", "Species", "Project", "date.harvest", "height.cm", "fresh.leaf.mass.g.or.mg", "fresh.leaf.mass.mg", "dry.leaf.mass.g.or.mg", "dry.leaf.mass.mg", "dry.shoot.mass.minus.leaf.g", "dry.shoot.mass.minus.leaf.mg", "dry.root.mass.g.or.mg", "dry.root.mass.mg", "Notes")

### lina ####
unique(lina_traits_fall$Species)

lina_sp_overlap <- c("Anagallis arvensis", "Avena barbata","Cerastium glomeratum", "Hypochaeris glabra", "Hypochaeris radicata")

fall_lina_sub <- lina_traits_fall %>%
  filter(Species %in% lina_sp_overlap)

colnames(fall_lina_sub)
colnames(fall_lina_sub) <- c("Rep", "Code", "Species", "Project", "date.harvest", "height.cm", "fresh.leaf.mass.g.or.mg", "fresh.leaf.mass.mg", "dry.leaf.mass.g.or.mg", "dry.leaf.mass.mg", "dry.shoot.mass.minus.leaf.g", "dry.shoot.mass.minus.leaf.mg", "dry.root.mass.g.or.mg", "dry.root.mass.mg", "Notes")


### caitlin ####
unique(caitlin_traits_fall$Species)
caitlin_sp_overlap <- c("Taenitherum caput-medusae")

fall_caitlin_sub <- caitlin_traits_fall %>%
  filter(Species %in% caitlin_sp_overlap)

colnames(fall_caitlin_sub) <- c("Rep", "Code", "Species", "Project", "date.harvest", "height.cm", "fresh.leaf.mass.g.or.mg", "fresh.leaf.mass.mg", "dry.leaf.mass.g.or.mg", "dry.leaf.mass.mg", "dry.shoot.mass.minus.leaf.g", "dry.shoot.mass.minus.leaf.mg", "dry.root.mass.g.or.mg", "dry.root.mass.mg", "Notes")

### carmen ####
colnames(carmen_traits_fall) <- c("Rep", "Code", "Species", "Project", "date.harvest", "height.cm", "fresh.leaf.mass.g.or.mg", "fresh.leaf.mass.mg", "dry.leaf.mass.g.or.mg", "dry.leaf.mass.mg", "dry.shoot.mass.minus.leaf.g", "dry.shoot.mass.minus.leaf.mg", "dry.root.mass.g.or.mg", "dry.root.mass.mg", "Notes")

### join together ####
fall_allsp <- do.call("rbind", list(fall_comp_sub, fall_caitlin_sub, fall_lina_sub, carmen_traits_fall))



## Check Spring ####
### compost/caitlin ####
colnames(compostcaitlin_traits_spring)


# Make g or mg Decisions ####
## we probably want to use mg for everything?
## not every value is in both the g/mg and mg columns...
ggplot(fall_allsp, aes(x=fresh.leaf.mass.g.or.mg, y=fresh.leaf.mass.mg)) +
  geom_point()

# Join Fall & Spring ####
