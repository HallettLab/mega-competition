## Ricker Model with Exponent Exploration

library(tidyverse)
theme_set(theme_bw())

#model1 <- exp(-a*g*N - a*g*(N^2))
#model2 <- exp(-a*g*N + a*g*(N^2))
#model3 <- exp(-a*g*N - (a*g*N)^2)
#model4 <- exp(-a*g*N + (a*g*N)^2)


popsize <- seq(100, 1000, by = 100)
alpha <- seq(-1, -0.1, by = 0.1)
#germ <- seq(0,1, by = 0.1)
germ <- 0.5

output <- data.frame(Pop = NA, Alpha = NA, Germ = NA, Output = NA)

for(N in popsize) {
  
  for (a in alpha) {
    
    for (g in germ) {
      
      test <- exp(-a*g*N - (a*g*N)^2)
      
      temp <- data.frame(Pop = NA, Alpha = NA, Germ = NA, Output = NA)
      temp$Pop <- N
      temp$Alpha <- a
      temp$Germ <- g
      temp$Output <- test
      
      output <- rbind(output, temp)
      
    }
  }
  
}

output <- output[-1,]

#output_1 <- output
#output_2 <- output
#output_3 <- output
#output_4 <- output


ggplot(output_1, aes(x=Pop, y=Output)) +
  geom_point() +
  #geom_line() +
  facet_wrap(~Alpha) +
  xlab("Nt") +
  ylab("Ricker Model Exponent")

ggplot(output_2, aes(x=Alpha, y=Output, color = as.factor(Pop))) +
  geom_point() +
  #geom_line() +
  facet_wrap(~Germ) +
  xlab("Nt") +
  ylab("Ricker Model Exponent")

ggplot(output_3[output_3$Pop != 100,], aes(x=Alpha, y=Output)) +
  geom_point() +
  #geom_line() +
  facet_wrap(~Pop, ncol = 5, nrow = 2) +
  xlab("Alpha") +
  ylab("Ricker Model Exponent")
