print(tmp_alphas)


## edge list
test = data.frame(V1 = c("ANAR", "BRHO", "CESO", "TACA", "ANAR", "BRHO", "ANAR", "CESO", "ANAR", "TACA", "BRHO", "CESO", "BRHO", "TACA", "CESO", "TACA"),
           V2 = c("ANAR", "BRHO", "CESO", "TACA", "BRHO", "ANAR", "CESO", "ANAR", "TACA", "ANAR", "CESO", "BRHO", "TACA", "BRHO", "TACA", "CESO"), 
           weight = rep(0.5, 16))


test2 = as.data.frame(tmp_alphas)

gtest = graph_from_adjacency_matrix(tmp_alphas, weighted = TRUE, mode = 'directed')
#dftest <- as_data_frame(gtest)

mod = cluster_spinglass(gtest, spins = 100, implementation = c("neg"))

print(mod)
membership(mod)
modularity(mod)


modularity_matrix()

myAdjacencyMatrix <- matrix(runif(400),nc=20,nr=20)

g  <- graph_from_adjacency_matrix(myAdjacencyMatrix,weighted=TRUE)

df <- as_data_frame(g)
head(df)
#   from to    weight
# 1    1  1 0.2655087
# 2    1  2 0.9347052
# 3    1  3 0.8209463
# 4    1  4 0.9128759
# 5    1  5 0.4346595
# 6    1  6 0.6547239