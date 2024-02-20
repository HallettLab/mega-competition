

library(reticulate)


use_python("C:\\Users\\carme\\AppData\\Local\\Programs\\Python\\Python311\\python.exe")


# loads the relevant python code
source_python("../NFD_definitions/numerical_NFD.py")

# create the differential equation system
n_spec <- 2 # number of species in the system, must be an integer
set.seed(0) # set random seed for reproduce ability

# Lotka-Volterra model
A <- matrix(runif(n_spec^2,0,1), n_spec, n_spec) # interaction matrix
diag(A) <- runif(n_spec, 1,2) # to ensure coexistence
mu <- runif(n_spec,1,2) # intrinsic growth rate
test_f <- function(N){
  return(mu - A%*%N)
}

pars <- NFD_model(test_f(A), n_spec, from_R = TRUE)
ND <- pars$ND
NO <- pars$NO
FD <- pars$FD
c <- pars$c