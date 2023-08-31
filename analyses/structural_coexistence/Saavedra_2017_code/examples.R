#R-code of "A structural approach for understanding multispecies coexistence" by:
#Serguei Saavedra, Rudolf P. Rohr, Jordi Bascompte, Oscar Godoy, Nathan J.B. Kraft,
#and Jonathan M. Levine
#published in: Ecological Monographs
setwd("~/Graduate School/Rotations/Winter2021/Saavedra_2017_code")

rm(list=ls())
source('toolbox_coexistence.R')
source('toolbox_figure.R')

#3-species interaction matrix of figure 5 and 6
alpha <- matrix(c(1,0.5,0.05,0.4,1,0.5,0.3,0.6,1),3,3)

#structural niche difference (Omega)
Omega(alpha)

#centroid of the feasibility domain
r_c <- r_centroid(alpha)

#structural fitness difference (theta), for a given vecotr of intrinsic growth rates (r = (1,1,1)), 
r <- c(1,1,0.5)
theta(alpha,r)

#test if the system (alpah,r) is feasible
test_feasibility(alpha,r)

#test the feasibility of all the pairs in the system (alpah,r)
test_feasibility_pairs(alpha,r)

#compute the overlap betwen the feasibility domain and the domain of coexistence of all the pairs (with 10000 randomizations)
compute_overlap(alpha,10000)


#draw the 3D cone of feasibility (like figure 4)
cone_3sp(alpha,'cone_3D.pdf',c('Sp1','Sp2','Sp3'))

#draw the projection of the feasibility of a 3-species system 
#on the simplex (like figure 5b and 6c)
projection_3sp(alpha,'projection_3D.pdf',c('Sp1','Sp2','Sp3'))

#draw the projection of the feasibility of a 3-species system 
#on the simplex, and overlap the domaine fo feasibility of all pairs (like figure 6d)
projection_3sp_with_pairwise(alpha,'projection_3D_with_pairs.pdf',c('Sp1','Sp2','Sp3')) # inputs = matrix of alphas, filename, species names

#4-species interaction matrix of figure 5 and the projection of its fesability
#domain on the simplex
alpha <- matrix(c(1, 0.5, 0.05, 0.2, 0.4, 1, 0.5, 0.2, 0.3, 0.6, 1, 0.2, 0.2, 0.2, 0.2, 1),4,4)
projection_4sp(alpha,'projection_4D.pdf',c('Sp1','Sp2','Sp3','Sp4'))
