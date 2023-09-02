## Trouble shooting

remove.packages("StanHeaders")
#remove.packages("rstan")
install.packages("https://cran.r-project.org/src/contrib/Archive/StanHeaders/StanHeaders_2.19.2.tar.gz",
                 type="source",repos=NULL)
packageVersion("StanHeaders")
##install.packages("rstan", type="source") ## downloads an old version of rstan - the latest available on CRAN

packageVersion("rstan")


install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
## loaded the most recent version of stan
install.packages("StanHeaders")


install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
