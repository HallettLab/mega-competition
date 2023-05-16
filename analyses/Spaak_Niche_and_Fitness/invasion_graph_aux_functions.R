# R Code for the manuscript "Building modern coexistence theory from the ground up: the role of community assembly" by Jurg Spaak and Sebastian Schreiber

# R Code Author: Sebastian J. Schreiber

# This file provides an alternative means of plotting invasion graphs. The R code in 10.5281/zenodo.7111753 provides basic functions for constructing the invasion graphs

######################
# THE plot.IG.alt FUNCTION
######################
# Inputs: the output from the IG.function command in 10.5281/zenodo.7111753
# bend.factor determines the convexity of the curve on which the vertices with the same species richess are plotted. Takes on values between 0 (no bending) to 1 (most bending)
# cols a vector of colors where
# cols[1] - edges S->T corresponding to invasions of more than 1 species
# cols[2] - edges S->T corresponding to single species invasions leading to a more specious community
# cols[3] - edges S->T corresponding to single species invasions leading to no change in number of species 
# cols[4] - edges S->T corresponding to single species invasions leading to a less specious community
# cols[5] - color of vertex background 
# cols[6] - color of vertex labels and vertex edge
# multiple.invasion.edge.weight - weight on the edges corresponding to invasions of more than 1 species

plot.IG.alt=function(out,bend.factor=0.75,cols=c("lightgray","blue","gold","red","white","black"),multiple.invasion.edge.weight=0.5){
  IG=out$IG
  IS=out$IS
  number.species=out$number.species
  permanent=out$permanent
  composition=out$composition
  not.permanent=which(!out$permanent)
  # create graph
  g=graph_from_adjacency_matrix(IG)
  # get edge vertex info: column 1 = tail, 2 = tip
  edges_data_frame <- get.data.frame(g, what = "edges")
  # create edge widths based on one versus multiple invasion
  for(i in 1:length(edges_data_frame[,1])){
    E(g)$weight[i]=multiple.invasion.edge.weight
    E(g)$color[i]=cols[1]
    SS=composition[[edges_data_frame[i,1]]]
    TT=composition[[edges_data_frame[i,2]]]
    if(length(setdiff(TT,SS))<2){
      E(g)$weight[i]=2
      E(g)$color[i]=cols[2]
      if(length(TT)==length(SS))E(g)$color[i]=cols[3]
      if(length(TT)<length(SS))E(g)$color[i]=cols[4]
      }
  }
  # create the reordered version of the graph (by edge weight that is used ultimately for the plotting
  g2 <- make_graph(as.vector(t(get.edgelist(g)[order(E(g)$weight),])))
  k=length(number.species) # number of communities
  # create vertex map
  xvals=numeric(k)
  yvals=numeric(k)
  lengths=numeric(max(number.species)+1)
  for(ii in 0:max(number.species)){
    lengths[ii+1]=length(which(number.species==ii))
  }
  counter=1
  for(i in 0:max(number.species)){
    for(j in 1:lengths[i+1]){
      xvals[counter]=j+(max(lengths)-lengths[i+1])/2
      x.temp=2*xvals[counter]/max(lengths)-1
      yvals[counter]=i-(1-(x.temp)^2)*bend.factor
      counter=counter+1}
  }
  
  # community names
  v.name=c(expression(symbol("\306")))
  for(i in 2:k){
    temp=composition[[i]]
    v.name=c(v.name,paste(unlist(temp),collapse=""))
  }

# colors and shape for the vertices
  
  vcols=rep(cols[5],k)
  vertex.frame.cols=rep(cols[6],k)
  vertex.label.cols=rep(cols[6],k)
  vertex.shapes=rep("circle",k)

# assign properties to the reordered graph g2
  E(g2)$color <- E(g)$color[order(E(g)$weight)]
  E(g2)$weight<-E(g)$weight[order(E(g)$weight)]
  V(g2)$name <-v.name
  par(mar=c(0,0,0,0))
  plot(g2,vertex.color=vcols,edge.width=E(g2)$weight,
       vertex.frame.color=vertex.frame.cols,vertex.shape=vertex.shapes,vertex.label.color=vertex.label.cols,layout=cbind(xvals,yvals),vertex.label.cex=0.5,edge.arrow.size=0.4)
}

######################
# Find End States
######################
# Input: output of the IG function
# Output: 
# end.states a vector of the end states i.e. -i communities where species i has a negative IGR
# nMinus1 a list of the "n-1" communities associated with the end states

FindEndStates=function(out){
  end.states=c()
  for(j in 1:length(out$minus.i)){
    who=out$minus.i[j]
    who=who[[1]]
    com=out$composition[[who]]
    if(max(out$IS[who,-com])<0){
      end.states=c(who,end.states)
    }
  }
  nMinus1=list()
  for(j in 1:length(end.states)){
    nMinus1[[j]]=out$composition[[end.states[j]]]
  }
  return(list(end.states=end.states,nMinus1=nMinus1))
}



######################
# THE FindCycles FUNCTION
######################
# Command for finding cycles in a cyclic graph
# Source: https://stackoverflow.com/a/55094319
# Input: a graph (igraph format)
# Output: some (but not necessarily all) of the directed cycles in the graph (see https://stackoverflow.com/a/55094319 for more details)
FindCycles = function(g) {
  Cycles = NULL
  for(v1 in V(g)) {
    if(degree(g, v1, mode="in") == 0) { next }
    GoodNeighbors = neighbors(g, v1, mode="out")
    GoodNeighbors = GoodNeighbors[GoodNeighbors > v1]
    for(v2 in GoodNeighbors) {
      TempCyc = lapply(all_simple_paths(g, v2,v1, mode="out"), function(p) c(v1,p))
      TempCyc = TempCyc[which(sapply(TempCyc, length) > 3)]
      TempCyc = TempCyc[sapply(TempCyc, min) == sapply(TempCyc, `[`, 1)]
      Cycles  = c(Cycles, TempCyc)
    }
  }
  Cycles
}

