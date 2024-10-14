## Script Purpose: set up functions to calculate network metrics

# Set up ####
library(tidyverse)
library(moments) ## good for skewness
#library(GiniWegNeg) # best definition of Gini with negative values
## this package has been archived and is no longer available
library(igraph)
library(bipartite)

# Dominance ####
## Function for calculating the global ratio of intraspecific vs. interspecific competition
## D = diagonal dominance of the matrix

## metric taken from Daniel et al. 2024

## Dominance = 1/n * sum( abs(alpha_ii) - sum(alpha_ij) )

dominance <- function(alpha){
  
  ## separate intra alphas
  ii = alpha[row(alpha) == col(alpha)]
  
  ## set alpha_ii's to 0 in the matrix to avoid messing up future calcs
  diag(alpha) = 0
  
  ## make a column of intra alphas in the matrix
  test = as.data.frame(cbind(ii, alpha))
  
  ## calc dominance index
  test2 = test %>%
    mutate(d = abs(test[,1]), ## get abs value of alpha_ii
           sp1 = abs(test[,2]), ## get abs value
           sp2 = abs(test[,3]), ## get abs value
           sp3 = abs(test[,4]), ## get abs value
           sp4 = abs(test[,5]), ## get abs value
           sp5 = abs(test[,6]),
           sp6 = abs(test[,7]),
           d2=rowSums(across(sp1:sp6)), ## sum all alpha_ij for a species
           dom = d - d2) ## calc dominance for a species
  
  return(mean(test2$dom))
  
}


# Asymmetry ####
## metric taken from Daniel et al. 2024 who cite Vasquez et al 2007

## A = sum(abs(alpha_ij) - abs(alpha_ji))/n

## need to be able to extract pairwise combinations of all sp in the network

## Important Q ####
## I wonder if we should also take the abs of the difference before summing together; otherwise the directionality of the pairs might matter?

asymmetry <- function(alpha, sp1, sp2, sp3, sp4, sp5, sp6, n){

  ## set alpha_ii's to 0 in the matrix to avoid messing up future calcs
  diag(alpha) = 0

  ## pull out pairwise interaction coefficients
  a_12 = alpha[sp1, sp2]  
  a_21 = alpha[sp2, sp1]
    
  a_13 = alpha[sp1, sp3]
  a_31 = alpha[sp3, sp1]
    
  a_14 = alpha[sp1, sp4]
  a_41 = alpha[sp4, sp1]
  
  a_15 = alpha[sp1, sp5]
  a_51 = alpha[sp5, sp1]
  
  a_16 = alpha[sp1, sp6]
  a_61 = alpha[sp6, sp1]
    
  a_23 = alpha[sp2, sp3]
  a_32 = alpha[sp3, sp2]
    
  a_24 = alpha[sp2, sp4]
  a_42 = alpha[sp4, sp2]
  
  a_25 = alpha[sp2, sp5]
  a_52 = alpha[sp5, sp2]
  
  a_26 = alpha[sp2, sp6]
  a_62 = alpha[sp6, sp2]
    
  a_34 = alpha[sp3, sp4]
  a_43 = alpha[sp4, sp3]
  
  a_35 = alpha[sp3, sp5]
  a_53 = alpha[sp5, sp3]
  
  a_36 = alpha[sp3, sp6]
  a_63 = alpha[sp6, sp3]
  
  a_45 = alpha[sp4, sp5]
  a_54 = alpha[sp5, sp4]
  
  a_46 = alpha[sp4, sp6]
  a_64 = alpha[sp6, sp4]
  
  a_56 = alpha[sp5, sp6]
  a_65 = alpha[sp6, sp5]
    
  ## calculate the asymmetry
  A = sum( abs(a_12 - a_21), 
           abs(a_13 - a_31),
           abs(a_14 - a_41),
           abs(a_15 - a_51),
           abs(a_16 - a_61),
           
           abs(a_23 - a_32),
           abs(a_24 - a_42),
           abs(a_25 - a_52),
           abs(a_26 - a_62),
           
           abs(a_34 - a_43),
           abs(a_35 - a_53),
           abs(a_36 - a_63),
           
           abs(a_45 - a_54),
           abs(a_46 - a_64),
           
           abs(a_56 - a_65)
           
        ) / n
   
    return(A)
    
}


# Skewness ####
skew = function(alpha) {
  
  diag(alpha) = NA
  
  sk = skewness(c(alpha[,1], alpha[,2], alpha[,3], alpha[,4], alpha[,5], alpha[,6]), na.rm = TRUE)
  
  #skewness(alpha)
  
  return(sk)
  
}

# Gini evenness ####
## SKIP ####
## will need to do more reading into this to determine whether there is a useful way to do this with negative numbers now that the GiniWegNeg package is no longer being maintained


# Modularity ####
mod = function(alpha) {
  
  g = graph_from_adjacency_matrix(tmp_alphas, weighted = TRUE, mode = 'directed')
  mod = cluster_spinglass(g, spins = 100, implementation = c("neg"))
  mod_value = modularity(mod)
  
  return(mod_value)
  
}



#test = graph_from_adjacency_matrix(tmp_alphas)

#modularity(x = , membership = ,)

## x = input graph (the matrix/network)
## membership vector fo the community structure; one value for each vertex
## weights = edge weights; use abs values of the coefficients as edge weights
## resolution = set to 1 to use classical definition of modularity


## Bimler et al. 2024, supplementary info page 15
    ## uses modularity from Traag & Bruggeman 2009
    ## uses cluster_spinglass() function from igraph, setting implementation to 'neg'





# Intransitivity ####

