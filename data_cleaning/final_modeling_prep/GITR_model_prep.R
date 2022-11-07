


## phyto seeds ####
# Predict Seed Num ####
gitr_final <- gitr_dat %>%
  mutate(predicted.flower.num = (0.3267 + (77.6127*total.biomass.rounded.percap) - (7.1135*(total.biomass.rounded.percap^2))),
         predicted.seed.num = ifelse(treatment == "D", predicted.flower.num*8.701754, predicted.flower.num*11.640625))