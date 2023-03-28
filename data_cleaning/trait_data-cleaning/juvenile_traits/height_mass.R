## Set up Env
library(tidyverse)
library(openxlsx)

# Read in data ####
## file path
lead <- "/Users/carme/Dropbox (University of Oregon)/Greenhouse_Traits/Data/"

## Fall ####
compost_traits_fall <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_fall2021.xlsx"), sheet = 1)

lina_traits_fall <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_fall2021.xlsx"), sheet = 2)

caitlin_traits_fall <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_fall2021.xlsx"), sheet = 3)

carmen_traits_fall <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_fall2021.xlsx"), sheet = 4)


## Spring ####
compost_caitlin_traits_spring <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_redo_spring2022.xlsx"), sheet = 1)

lina_traits_spring <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_redo_spring2022.xlsx"), sheet = 2)

carmen_traits_spring <- read.xlsx(paste0(lead, "Entered_Data/traits_measured/greenhouse_trait_datasheets_redo_spring2022.xlsx"), sheet = 3)


# Merge Fall Traits ####
## Fix Col Names ####
colnames(compost_traits_fall)
unique(compost_traits_fall$X16)
## rename notes col to match others, get rid of column X16
compost_traits_fall2 <- compost_traits_fall %>%
  mutate(Notes = notes) %>%
  select(-notes, -X16)
colnames(compost_traits_fall2)


colnames(carmen_traits_fall) == colnames(lina_traits_fall)
## mismatches b/w cols 9, 11, and 13
colnames(compost_traits_fall2) == colnames(lina_traits_fall)
## mismatches b/w cols 9, 11, and 13
colnames(carmen_traits_fall) == colnames(compost_traits_fall2)
## mismatch between col 11


## col 9 
# carmen/compost: "dry.leaf.mass.(g.or.mg)", lina: "dry.leaf.mass.(g)", compost: 
## col 11
# carmen: "dry.shoot.mass.(-leaf).(g.or.mg)", lina: "dry.shoot.mass.(-leaf).(g.)", compost: "dry.shoot.mass.(-leaf).(g)"
## col 13
## carmen/compost: "dry.root.mass.(g.or.mg)", lina: "dry.root.mass.(g)"

unique(carmen_traits_fall$`dry.leaf.mass.(g.or.mg)`)
## seems like there are values that probably are in both g and mg
unique(carmen_traits_fall$`dry.shoot.mass.(-leaf).(g.or.mg)`)
## hard to tell one way or the other?

ggplot(carmen_traits_fall, aes(x=`dry.shoot.mass.(-leaf).(g.or.mg)`, fill = Species)) +
  geom_histogram()


## Merge ####
fall_traits <- do.call("rbind", list(carmen_traits_fall, caitlin_traits_fall))

## Decide g or mg ####



# Merge Spring Traits ####
### Check Col Names ####
colnames(compost_caitlin_traits_spring) == colnames(lina_traits_spring)
colnames(compost_caitlin_traits_spring) == colnames(carmen_traits_spring)
## all match, that part looks ok

head(compost_caitlin_traits_spring)
## these are missing species name in a lot of cases

## all of the spring data are, the species code only appears in the first row and then the following 5 reps also are the same species, it's just not marked. ugh. 

## Fill in Species Col ####
## try out the fill function
compost_caitlin_traits_spring2 <- compost_caitlin_traits_spring %>% 
  fill(Code, .direction = "down")

compost_caitlin_traits_spring$Code
compost_caitlin_traits_spring2$Code

## Based on how the excel file looked, it should be sufficient to fill down

head(carmen_traits_spring)

carmen_traits_spring2 <- carmen_traits_spring %>%
  fill(Code, .direction = "down")

carmen_traits_spring$Code
carmen_traits_spring2$Code

## it seems like there are 2 extra rows of TRWIf added that have values for only the dry shoot mass and dry root mass but nothing else... not sure why these apparent duplicates are here? They share the same code and rep number with other rows that are fully filled out.

head(lina_traits_spring)

lina_traits_spring2 <- lina_traits_spring %>%
  fill(Code, .direction = "down")

lina_traits_spring$Code
lina_traits_spring2$Code

## Merge ####
spring_traits <- do.call("rbind", list(compost_caitlin_traits_spring2, lina_traits_spring2, carmen_traits_spring2))

## Fix Date Format ####
spring_traits$date.harvest <- as.Date(spring_traits$date.harvest, origin = "1899-12-30")
## double check
unique(spring_traits$date.harvest)

## Fix Col Names ####
colnames(spring_traits) <- c("Rep", "Code", "Project", "date.harvest", "height.cm", "fresh.leaf.mass.g", "fresh.leaf.mass.mg", "dry.leaf.mass.g", "dry.leaf.mass.mg", "dry.shoot.mass.-leaf.g", "dry.shoot.mass.-leaf.mg", "dry.root.mass.g", "dry.root.mass.mg", "Notes", "Leaf.scanned")


colnames(spring_traits)

ggplot(spring_traits, aes(x=fresh.leaf.mass.g)) +
  geom_histogram() +
  facet_wrap(~Code)
ggplot(spring_traits, aes(x=fresh.leaf.mass.mg)) +
  geom_histogram()
## empty

str(spring_traits)

# Join Fall & Spring ####





