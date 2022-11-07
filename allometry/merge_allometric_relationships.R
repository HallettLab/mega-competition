## Merge allometric relationships

## the purpose of this script is to put all allometric relationships in one place so that they can be easily sourced and used in prepping the phyto & background data for modeling. 

## read in allometric relationships
source("allometry/gitr_allometry_testing.R")
source("allometry/brho_allometry_testing.R")


## make a vector of rel names
allo.outputs <- c("gitr.allo.output", "brho.allo.output")

allo.df <- as.data.frame(matrix(ncol = 4))
colnames(allo.df) <- c("species", "intercept", "coeff", "coeff2")

for (i in 1:length(allo.outputs)) {
  
  sp <- get(allo.outputs[i])
  temp <- data.frame(species = NA, intercept = NA, coeff = NA, coeff2 = NA)
  temp$species <- substr(allo.outputs[i], 1, 4)
  temp$intercept <- sp[1]
  temp$coeff <- sp[2]
  temp$coeff2 <- sp[3]
  
  allo.df <- rbind(allo.df, temp)
}
