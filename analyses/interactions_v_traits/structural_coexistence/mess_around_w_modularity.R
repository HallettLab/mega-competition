library(igraph)


str(el.nw.C)

test_graph = graph_from_data_frame(el.nw.C)
str(test_graph)


g<-graph_from_data_frame(net) # builds a network with all possible paths


weight<-c(t(subalpha)) # saves the interaction coefficient in a variable

E(g)$weight<-weight # assigns interaction coefficient as weights of paths
clu<-cluster_optimal(g,weights = abs(E(g)$weight)) # defines which are the best clusters
mod<-modularity(g,membership(clu), weights = abs(E(g)$weight)) # calculates modularity with absolute values of interaction coefficients as weight


E(test_graph)$weight

cluster = cluster_optimal(test_graph)


modularity(test_graph, membership = cluster)
