

netmet = read.csv("analyses/interactions_v_traits/structural_coexistence/calc_comm_attributes/6_sp_network_metrics_20241008.csv")


names(netmet) = c("dominance", "asymmetry", "skewness", "modularity", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "rainfall", "draw", "iteration")


hist(netmet$dominance)
hist(netmet$asymmetry)
hist(netmet$skewness)
hist(netmet$modularity)
