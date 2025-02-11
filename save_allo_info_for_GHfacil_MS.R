

source("allometry/acam_allometry_testing.R")
source("allometry/brho_allometry_testing.R")

GH_facil = rbind(ACAM.allo.output, BRHO.tb.allo.output)

write.csv(GH_facil, "data/allometry_for_GH_facilMS.csv")
