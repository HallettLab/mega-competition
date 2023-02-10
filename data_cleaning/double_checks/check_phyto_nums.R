source("data_cleaning/phyto-processing_data-cleaning/compile_phyto-processing_data.R")

source("data_cleaning/phyto-collections_data-cleaning/phyto-collections_data-cleaning.R")

## compare phyto # between all.phytos and collectionsC
## this doesn't quite work, phyto.census already has removed 0's from the phyto.n.indiv so we miss any that are marked as 0 in the collections data.

coll_proc <- left_join(all.phytos, phyto.census, by = "unique.ID")

ggplot(coll_proc, aes(x=phyto.n.indiv.x, y=phyto.n.indiv.y)) +
  geom_point()

mismatch <- coll_proc %>%
  filter(phyto.n.indiv.x != phyto.n.indiv.y)
## 4230 corrected on Google Drive; change will eventually be moved to DropBox.

unique(coll_proc$phyto.n.indiv.x)
unique(coll_proc$phyto.n.indiv.y)

na.check <- coll_proc %>%
  filter(is.na(phyto.n.indiv.y))
## two AMME samples don't have unique IDs... better check this in the cleaning!!