## Interaction Networks

## Goals: visualize networks in control & drought treatments, relate plant traits to networks somehow, measure network properties

# Set up ###
library(tidyverse)
library(qgraph)
library(igraph)

## read in data
sums.info <- read.csv("data/parameter_summaries_20240714_models.csv")

# Format data ###
## try data format as edgelist
## control data
el.nw.C <- sums.info %>%
  filter(parameter_type != "lambda", treatment == "C") %>%
  select(-mean_parameter, -hdi_lo, -hdi_hi, -treatment, -X) %>% ## keep only median alphas, discard rest of info
  mutate(alpha = toupper(substr(parameter_type, 7, 10))) %>%
  select(alpha, species, median_parameter) %>%
  mutate(species = fct_relevel(species, "ACAM", "TWIL", "THIR", "BRHO", "LOMU", "TACA", "AMME", "GITR", "LENI", "MAEL", "MICA", "PLER", "PLNO", "CESO", "ANAR", "BRNI"))

grpsC <- c("native_legume", "native_forb", "non-native_forb", "non-native_forb", "native_forb", "native_forb", "non-native_grass", "native_forb", "native_forb", "native_forb", "native_forb", "non-native_grass", "native_legume", "non-native_grass", "non-native_forb", "non-native_legume")

png("analyses/interactions_v_traits/median_params/preliminary_figs/network_C_circlelayout.png", width = 2000, height = 1400, units = 'px')

qgraph(el.nw.C, 
       layout = 'circle',
       groups = grpsC,
       negCol = '#5D69B1',   # facilitation = blue
       posCol = '#E58606',
       asize = 4,
       vsize = 4,
       vTrans = 100,
       fade = T,
       trans = 0.5,
       maximum = 0,
       color = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E"),
      # title = "Control",
       GLratio = 4,
       legend.cex = 1.5
      )

dev.off()
#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#ED645A,#CC3A8E,#A5AA99
## visualize network
png("analyses/interactions_v_traits/median_params/preliminary_figs/network_C.png", width = 1200, height = 1200, units = 'px')

qgraph(el.nw.C, 
       layout = 'circle',
       negCol = '#78d0fd',   # facilitation = blue
       posCol = '#fda578',
       asize = 6,
       vTrans = 100,
       fade = T,
       trans = 0.5,
       maximum = 0)
dev.off()

## drought data 
el.nw.D <- sums.info %>%
  filter(parameter_type != "lambda", treatment == "D") %>%
  select(-mean_parameter, -hdi_lo, -hdi_hi, -treatment, -X) %>%
  mutate(alpha = toupper(substr(parameter_type, 7, 10))) %>%
  select(alpha, species, median_parameter) %>%
  mutate(alpha = fct_relevel(alpha, "ACAM", "TWIL", "THIR", "BRHO", "LOMU", "TACA", "AMME", "GITR", "LENI", "MAEL", "MICA", "PLER", "PLNO", "CESO", "ANAR", "BRNI")) %>%
  mutate(species = fct_relevel(species, "ACAM", "TWIL", "THIR", "BRHO", "LOMU", "TACA", "AMME", "GITR", "LENI", "MAEL", "MICA", "PLER", "PLNO", "CESO", "ANAR", "BRNI"))

## visualize network
png("analyses/interactions_v_traits/median_params/preliminary_figs/network_D.png", width = 1200, height = 1200, units = 'px')
qgraph(el.nw.D, 
       layout = 'circle',
       negCol = '#78d0fd',   # facilitation = blue
       posCol = '#fda578',
       asize = 6,
       vTrans = 100,
       fade = T,
       trans = 0.5, maximum = 0)
dev.off() 


grpsD <- c("native_legume", "native_forb", "non-native_forb", "non-native_grass", "non-native_forb", "non-native_forb", "native_forb", "native_forb", "non-native_grass", "native_forb", "native_forb",  "non-native_grass", "non-native_legume", "native_legume", "native_forb", "native_forb")



png("analyses/interactions_v_traits/median_params/preliminary_figs/network_D_circlelayout.png", width = 2000, height = 1400, units = 'px')

qgraph(el.nw.D, 
       layout = 'circle',
       groups = grpsD,
       negCol = '#5D69B1',   # facilitation = blue
       posCol = '#E58606',
       asize = 4,
       vsize = 4,
       vTrans = 100,
       fade = T,
       trans = 0.5,
       maximum = 0,
       color = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E"),
       # title = "Control",
       GLratio = 4,
       legend.cex = 1.5
)

dev.off()

## might need to set the 'strongest' edge in order to compare between graphs?
## could eventually group nodes by functional group?

# Network metrics ####
## Modularity ####
### cluster_spinglass()

nwC <- graph_from_data_frame(el.nw.C, directed = TRUE)

cluster_spinglass(nwC)

nwD <- graph_from_data_frame(el.nw.D, directed = TRUE)

cluster_spinglass(nwD)

## overall low modularity - 0.25 for both
## not sure if the way I made the igraph object is completely correct
## take this result with a grain of salt










# OLD, DIDN'T WORK ####
nw.matrix.C <- sums.info %>%
  filter(parameter_type != "lambda", treatment == "C") %>%
  select(-mean_parameter, -hdi_lo, -hdi_hi, -treatment, -X) %>%
  pivot_wider(names_from = "species", values_from = "median_parameter") %>%
  mutate(alpha = toupper(substr(parameter_type, 7, 10))) %>%
  select(-parameter_type) %>%
  column_to_rownames("alpha") %>%
  as.matrix()

nw.matrix.C <- replace(nw.matrix.C, is.na(nw.matrix.C), 0)

t<-qgraph(nw.matrix.C,
       layout = 'circle',
       negCol = '#78d0fd',   # facilitation = blue
       posCol = '#fda578',
       asize = 6,
       vsize = 10,
       edge.width = 2,
       fade = T, directed = T,
       diag = T,
       edge.labels = TRUE)

as_edgelist(test2)
## some of the interaction coefficients are just not right on these labels. Don't know why it is not matching up the rownames and colnames?

nw.matrix.D <- sums.info %>%
  filter(parameter_type != "lambda", treatment == "D") %>%
  select(-mean_parameter, -hdi_lo, -hdi_hi, -treatment, -X) %>%
  pivot_wider(names_from = "species", values_from = "median_parameter") %>%
  mutate(alpha = toupper(substr(parameter_type, 7, 10))) %>%
  select(-parameter_type) %>%
  column_to_rownames("alpha") %>%
  as.matrix()

qgraph(nw.matrix.D,
       layout = 'circle',
       negCol = '#78d0fd',   # facilitation = blue
       posCol = '#fda578',
       asize = 6,
       vsize = 10,
       edge.width = 2,
       fade = T, directed = T,
       diag = T, edge.labels = TRUE)



test1 <- unname(nw.matrix.C)
test2 <- graph_from_adjacency_matrix(test1)

E(test2)$weight <- as.numeric(test1[!is.na(test1)])


test <- graph_from_adjacency_matrix(nw.matrix.C)

plot(test, layout = layout.circle, edge.curved=.1)



g <- make_graph(edges = c(1, 2, 1, 5), n = 10, directed = FALSE)
g
plot(g)


g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 

plot(g1)
