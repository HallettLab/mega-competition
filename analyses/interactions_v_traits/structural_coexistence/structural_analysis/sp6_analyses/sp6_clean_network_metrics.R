## Clean Network Metrics data for 6sp

## load packages
library(tidyverse)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## read in data
net_file_path = "analyses/interactions_v_traits/structural_coexistence/calc_comm_attributes/network_metrics/"

net6_1 = read.csv(paste0(net_file_path, "6_sp_network_metrics_20241009.csv"))
net6_2 = read.csv(paste0(net_file_path, "6_sp_network_metrics_part2_20241009.csv"))
net6_3 = read.csv(paste0(net_file_path, "6_sp_network_metrics_part3_20241009.csv"))

## trim net6_1 data as there are a number of repeat comms with net6_2
net6_1cut = net6_1[1:539800,]
## need communities 1:2699 from this 
## that would be: 2699*200 = 539800 write cntr

## bind all df's together
net6all = rbind(net6_1cut, net6_2, net6_3)

## check for 16016 comms w/100 iterations each
check_complete = net6all %>%
  group_by(rainfall, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  summarise(num_iter = n())
## looks good..

hist(net6all$dominance)
hist(net6all$asymmetry)
hist(net6all$skewness)
hist(net6all$modularity)

## summarise network metrics- 1/community comp
sp6net = net6all %>%
  group_by(rainfall, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  summarise(mean_dom = mean(dominance),
            mean_asym = mean(asymmetry),
            mean_skew = mean(skewness),
            mean_mod = mean(modularity),
            
            se_dom = calcSE(dominance),
            se_asym = calcSE(asymmetry),
            se_skew = calcSE(skewness),
            se_mod = calcSE(modularity)) 

## get rid of all extra dataframes
rm(check_complete, net6_1, net6_1cut, net6_2, net6_3)
