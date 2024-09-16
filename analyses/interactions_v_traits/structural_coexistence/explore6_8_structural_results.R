sp6 = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/structural_results_files/6_sp_structural_results_20240828.csv")

comp6 = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/community_permutations/comp6.csv")

comp8 = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/community_permutations/comp8.csv")

comp10 = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/community_permutations/comp10.csv")


sp8 = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/structural_results_files/8_sp_structural_results_20240828.csv")

unique(sp6$feasibility)
unique(sp8$feasibility)


286397/2
143198.5/100
## got to community 1431

## maybe try running in batches of 1000 or 1500?

12870-1431
## 11439

11439/1500

11439/2000

hist(sp8$feasibility)
max(sp8$iteration_num)

8008*2*100
368664/1601600
## 23% done only

(368664/2)/100


comp6new = comp6[1844:8008,]
comp6[1843,]
368663/1843


1844*2*100
