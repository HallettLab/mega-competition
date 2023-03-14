## traits data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Traits/")){
  # Carmen
  traits.lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Traits/"
  
} else {
  # Marina
  traits.lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Traits/"
} 

a.traits <- read.csv(paste0(traits.lead, "Megacomp_adult-traits.csv")) %>%
  select(Species_Name:CN) %>%
  mutate(FunGroup = paste(nativity, growth_form, sep = "_"))
