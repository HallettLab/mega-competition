## Calculating niche & fitness diffs b/w pairs


colnames(alphas_post)

Ndiff_mc_bh <- alphas_post %>%
  filter(species %in% c("BRHO", "MICA")) %>%
  select(species, alpha_brho_c, alpha_mica_c, alpha_brho_d, alpha_mica_d) #%>%
  #group_by(species) %>%

## i = BRHO
## j = MICA


Ndiff <- sqrt((mean(Ndiff_mc_bh[Ndiff_mc_bh$species == "BRHO",]$alpha_mica_c) / 
                mean(Ndiff_mc_bh[Ndiff_mc_bh$species == "MICA",]$alpha_mica_c)) *
                
                (mean(Ndiff_mc_bh[Ndiff_mc_bh$species == "MICA",]$alpha_brho_c) / 
                  mean(Ndiff_mc_bh[Ndiff_mc_bh$species == "BRHO",]$alpha_brho_c)))

