# R Code for the manuscript "Building modern coexistence theory from the ground up: the role of community assembly" by Jurg Spaak and Sebastian Schreiber

# R Code Author: Sebastian J. Schreiber

# This file illustrates how to use the R functions associated with creating and plotting invasion graphs.

# Source the basic functions for constructing the invasion graphs (original source: 10.5281/zenodo.7111753). 
source("models/CW/Spaak_Niche_and_Fitness/invasion_graph_main_functions.R")
# Source the auxiliary invasion graph functions
source("models/CW/Spaak_Niche_and_Fitness/invasion_graph_aux_functions.R")

# For illustration purposes, we use the Lotka-Volterra (LV) model presented in Figure 3 of the manuscript. In this LV model, dx/dt=x*(A%*%x+b) where x is a 6x1 vector of species densities, A is a 6x6 matrix of species interaction coefficients, and b is a vector of intrinsic rates of growths.

# These matrices are defined by 
A=-as.matrix(read.csv(file="models/CW/Spaak_Niche_and_Fitness/Geijzendorffer 2011.csv",header = FALSE))
b=rep(1,6)

# The command LV.IS creates the invasion scheme for this Lotka-Volterra model 

IS=LV.IS(A,b)

# For the example, the invasion scheme is a 37x6 matrix where the rows correspond to all feasible equilibria of the Lotka-Volterra model and the columns correspond to the 6 species. 

# We apply the IG.function to the invasion scheme to produce the complete invasion graph (i.e. including the community of all n=6 species) as well as additional information

out=IG.function(IS)

# The adjacency matrix of the invasion graph is given by out$IG. As there are 37 feasible equilibria, this matrix is 37x37

out$IG

# Lets plot this invasion graph. For this plotting command light gray arrows correspond to directed edges involving invasions of multiple species, blue arrows correspond to directed edges involving single species invasions resulting in increased species richness, red arrows correspond to directed edges involving single species invasions resulting in decreased species richness, and yellow arrows correspond to directed edges involving single species invasions with no change in species richness.

plot.IG.alt(out)

# Whether or not the invasion scheme is acyclic is given by the Boolean

out$acyclic

# As the Boolean is TRUE, the  invasion graph is acyclic. 

# If one only wants to know what all the end states for community assembly are, skip for a short cut on line XXX. Otherwise the following lines highlight more of the information contained in the output from the IG.function. 

#As the invasion graph is acyclic, we can identify all of the permanent communities by checking the Boolean out$permanent

out$permanent

# As all the entries are TRUE for this invasion graph, all feasible equilibria correspond to a permanent communities. Un general, this need not be true. 

# The number of species in the 37 communities are given by 

out$number.species

# As no permanent community includes 6 species, the 6 species community does not coexist (in the sense of permanence). However, we have 3 communities with 5 species. The composition of these communities can be identified by out$composition or looking at the plot. 

out$composition[35:37]

# In this case, the 5 species communities are {1,2,4,5,6} (i.e. a -3 community), {1,3,4,5,6} (i.e. a -2 community), and {2,3,4,5,6} (i.e. a -1 community)

# We can verify that each of these 5 species communities are invasion resistant by checking the invasion growth rate of each of the missing species. 

IS[35,3]
IS[36,2]
IS[37,1]

# Hence, only the {1,2,4,5,6} is an invasion resistant community i.e. an end state for community assembly. 

# If one only wants to know what all the end states for community assembly are, one can skip lines 44-66 and simply use the following command that returns the 

FindEndStates(out=out)

# To redo some of the analysis restricted to the  {1,2,4,5,6} community, we redefine the matrices A and b

A=A[-3,-3]
b=b[-3]

# and recompute the invasion scheme and invasion graph

ISno3=LV.IS(A,b) # 25x25 matrix
outno3=IG.function(ISno3)
plot.IG.alt(outno3)

# recheck acyclic (not necessary as full community invasion graph was acyclic) 
outno3$acyclic
# Check permanence and species richness
outno3$permanent
outno3$number.species
# Consistent with the earlier calculations, the community consisting of the 5 species is permanent. 

# To identify the n-1=4 communities of the n=5 community, we check the composition of the communities 4 species i.e. 22,23,24

outno3$composition[22:24]

# Hence, the n-1=4 communities are {1,2,3,5}, {1,3,4,5}, {2,3,4,5} i.e. {1,2,4,6},{1,4,5,6},{2,4,5,6} with the original numbering of the 6 species community.

