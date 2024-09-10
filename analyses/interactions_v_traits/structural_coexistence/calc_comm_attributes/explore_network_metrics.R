## explore network metrics

theme_set(theme_classic())

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## read in data
sp4_net = read.csv("analyses/interactions_v_traits/structural_coexistence/calc_comm_attributes/4_sp_network_metrics_20240905.csv", col.names = c("dominance", "asymmetry", "skewness", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "rainfall", "draw", "iteration_num"))

## fix first column in dataframe
names(sp4_net)


## -0.150443275	0.092016156	2.419013272	0	0	1	1	0	1	1	0	0	0	0	0	0	0	0	0	C	3194	0 
     ## copy & paste first row of data that gets left out as it is turned into column names by default when reading data in


row1 = data.frame(dominance = -0.150443275, asymmetry = 0.092016156, skewness = 2.419013272, 
                  ACAM = 0,	AMME = 0,	ANAR = 1,	BRHO = 1,	BRNI = 0,	CESO = 1,	GITR = 1,	LENI = 0,	LOMU = 0,	MAEL = 0, MICA = 0, PLER = 0,	PLNO = 0,	TACA = 0, THIR =	0, TWIL =	0, rainfall = "C", draw = 3194, iteration_num = 0)


netmet = rbind(row1, sp4_net)

hist(netmet$dominance) ## no variation in this metric, seems like something went wrong in calculation

hist(netmet$asymmetry)

hist(netmet$skewness)

ggplot(netmet, aes(x=skewness)) +
  geom_histogram() +
  facet_wrap(~rainfall)

netsums = netmet %>%
  mutate(comp = paste0(ACAM ,	AMME,	ANAR,	BRHO,	BRNI,	CESO,	GITR,	LENI,	LOMU,	MAEL, MICA, PLER,	PLNO,	TACA, THIR, TWIL)) %>%
  group_by(comp, rainfall, ACAM ,	AMME,	ANAR,	BRHO,	BRNI,	CESO,	GITR,	LENI,	LOMU,	MAEL, MICA, PLER,	PLNO,	TACA, THIR, TWIL) %>%
  summarise(mean_asym = mean(asymmetry, na.rm = T), 
            se_asym = calcSE(asymmetry),
            mean_skew = mean(skewness, na.rm = T),
            se_skew = calcSE(skewness))

ggplot(netsums, aes(x=mean_skew)) +
  geom_histogram() +
  facet_wrap(~rainfall)

