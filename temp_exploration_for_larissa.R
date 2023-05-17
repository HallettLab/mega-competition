## explore density & facilitation

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

brho_filtered <- brho.phyto %>%
  filter(bkgrd == "Control" | bkgrd == "PLER" | bkgrd == "BRHO" | bkgrd == "TWIL")


summary <- brho_filtered %>%
  group_by(bkgrd, treatment) %>%
  summarise(mean.seeds = mean(phyto.seed.out), se.seeds = calcSE(phyto.seed.out))


ggplot(summary, aes(x=bkgrd, y=mean.seeds, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.seeds - se.seeds, ymax = mean.seeds + se.seeds), width = 0.25)


m1 <- aov(phyto.seed.out ~ bkgrd+treatment, data = brho_filtered)
summary(m1)
## significant effect of bkgrd, no effect of treatment and no interaction
TukeyHSD(m1)

## signif comparisons
## Control-BRHO
## TWIL-BRHO
## TWIL-PLER




