
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
           d2=rowSums(across(sp1:sp4)), ## sum all alpha_ij for a species
           dom = d - d2) ## calc dominance for a species
  
  return(mean(test2$dom))
  
}


# Asymmetry ####
## metric taken from Daniel et al. 2024 who cite Vasquez et al 2007

## A = sum(abs(alpha_ij) - abs(alpha_ji))/n

## need to be able to extract pairwise combinations of all sp in the network

## Important Q ####
## I wonder if we should also take the abs of the difference before summing together; otherwise the directionality of the pairs might matter?

asymmetry <- function(alpha, sp1, sp2, sp3, sp4, n){

  ## set alpha_ii's to 0 in the matrix to avoid messing up future calcs
  diag(alpha) = 0
    
  ## pull out pairwise interaction coefficients
  a_12 = alpha[sp1, sp2]  
  a_21 = alpha[sp2, sp1]
    
  a_13 = alpha[sp1, sp3]
  a_31 = alpha[sp3, sp1]
    
  a_14 = alpha[sp1, sp4]
  a_41 = alpha[sp4, sp1]
    
  a_23 = alpha[sp2, sp3]
  a_32 = alpha[sp3, sp2]
    
  a_24 = alpha[sp2, sp4]
  a_42 = alpha[sp4, sp2]
    
  a_34 = alpha[sp3, sp4]
  a_43 = alpha[sp4, sp3]
    
  ## calculate the asymmetry
  A = sum( (abs(a_12) - abs(a_21)), 
        (abs(a_13) - abs(a_31)),
        (abs(a_14) - abs(a_41)),
        (abs(a_23) - abs(a_32)),
        (abs(a_24) - abs(a_42)),
        (abs(a_34) - abs(a_43)) ) / n
   
    return(A)
    
}


# Skewness ####
skew = function(alpha) {
  
  diag(alpha) = NA
  
  sk = skewness(c(alpha[,1], alpha[,2], alpha[,3], alpha[,4]), na.rm = TRUE)
  
  return(sk)
  
}

# Gini evenness ####
## SKIP ####
## will need to do more reading into this to determine whether there is a useful way to do this with negative numbers now that the GiniWegNeg package is no longer being maintained


# Modularity ####
# test = graph_from_adjacency_matrix(alpha)

# Intransitivity ####

