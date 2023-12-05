model.dat <- read.csv("data/model_dat.csv")

i <- "BRHO"
dat <- subset(model.dat, phyto == i)

Fecundity <- as.integer(round(dat$phyto.seed.out)) ## seeds out

hist(Fecundity)

a <- .001
eps <- rgamma(1000, a, a)

sig <- rgamma(1000, eps, eps)
mean(sig)


plot(sig)
hist(sig)
hist(log(rgamma(10000, 0.001, 0.001 )))

b <- 0.001
lambda <- rexp(1000, b)
plot(lambda)

hist(lambda)
