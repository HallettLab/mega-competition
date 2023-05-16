# R Code for the manuscript "Building modern coexistence theory from the ground up: the role of community assembly" by Jurg Spaak and Sebastian Schreiber

# R Code Author: Sebastian J. Schreiber

# This file produces the figure for Box 1. 

# load the deSolve packages
library(deSolve)

# define the seasonally forced predator-prey system 
f=function(t,x,parms){
  with(parms,{
    n=x[1]
    p=x[2]
    dn<-r*n*(1-n/K*(1+0.5*sin(0.1*t)))-a*n*p/(1+a*h*n)
    dp<-c*a*n*p/(1+a*h*n)-d*p
    list(c(dn,dp))
  })
}

# define the parameters and initial conditions for the model
parms=list(r=0.1,K=100,a=0.02,h=1,c=0.5,d=0.15)
x0=c(parms$K,0.1)
times=seq(0,1000,by=1)

# run the model to get rid of transients
out=ode(y = x0,times = times,func = f,parms=parms)
# use the end point of the initial simulation for the initial condition of the final simulation 
x0=out[1000,2:3]
times=seq(0,5000,by=0.1)
out=ode(y = x0,times = times,func = f,parms=parms)

# create the plots
TT=10000
n=out[1:TT,2]
p=out[1:TT,3]
times=times[1:TT]
# for the PDF use 5 by 4
layout(mat = cbind(c(1,3),c(2,4)),widths = c(2,1),heights = c(1.5,1))
par(mar=c(0,4,1,0))
matplot(times,cbind(n,p),type="l",xlab="",ylab="density",lty=1,lwd=2,xaxt="n")
breaks=seq(min(out[,2:3])*0,max(out[,2:3]),length=50)
outy=hist(out[,3],plot = FALSE,breaks=breaks)
outx=hist(out[,2],plot = FALSE,breaks=breaks)
par(mar=c(0,0,1,3))
barplot((outx$density),horiz = TRUE,xaxt="n",col = "gray",border = "darkgray")
barplot((outy$density)/2.25,horiz = TRUE,xaxt="n",add=TRUE,col="red",border="firebrick")
text(0.02,20,expression(p[S](dN[1])))
text(0.02,5,expression(p[S](dN[2])))


par(mar=c(4,4,1,0))
K=function(t)(1+0.5*sin(0.1*t))*parms$K
Ks=K(times)
plot(times,Ks,type ="l",xlab="time t",ylab="carrying capacity K",lwd=2)
breaks=seq(min(Ks),max(Ks),length=50)
outK=hist(Ks,plot = FALSE,breaks=breaks)
par(mar=c(4,0,1,1.25))
barplot((outK$density),horiz = TRUE,xaxt="n",col = "gray",border = "darkgray")
text(0.02,25,expression(p[S](dK)))

