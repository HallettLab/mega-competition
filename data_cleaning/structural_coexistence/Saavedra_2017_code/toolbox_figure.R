#R-code of "A structural approach for understanding multispecies coexistence" by:
#Serguei Saavedra, Rudolf P. Rohr, Jordi Bascompte, Oscar Godoy, Nathan J.B. Kraft,
#and Jonathan M. Levine
#published in: Ecological Monographs

require('scatterplot3d')
require('pracma')

######################
#R-code to draw the feasibility domain and its projection on the simplex.
#inputs: alpha = interaction matrix, file_name = output name of the figure (.pdf),
#sp_names = the names of the species.

#######################
cone_3sp <- function(alpha,file_name,sp_name){

  graphics.off()
  pdf(file=file_name, width = 12, height = 12, useDingbat=FALSE, family = "ArialMT")
  par(mar = c(0,0,0,0))

  D <- diag(1/sqrt(diag(t(alpha)%*%alpha))) #diag() extracts the diagonal of a matrix 
  alpha_n <- alpha %*% D # %*% means matrix multiplication
  
  v1 <- alpha_n[,1]
  v2 <- alpha_n[,2]
  v3 <- alpha_n[,3]
  vc <- (v1 + v2 + v3)
  vc <- vc / sqrt(sum(vc^2))

  lambda = c(0,1.2)

  X <- v1[1] * lambda 
  Y <- v1[2] * lambda 
  Z <- v1[3] * lambda 
  s3d <- scatterplot3d(X, -Y, Z, xlim=c(0,1.4), ylim=c(0,-1.4), zlim=c(0,1.4),type = 'l',
                    box = F, angle = 30, axis = F,grid=F)

  s3d$points3d(c(0,0),c(0,0),c(0,1.4),type='l',col='black',lwd=2) 
  s3d$points3d(c(0,0),c(0,-1.4),c(0,0),type='l',col='black',lwd=2) 
  s3d$points3d(c(0,1.4),c(0,0),c(0,0),type='l',col='black',lwd=2) 

  pp <- s3d$xyz.convert(0.62,0,0)
  text(x = pp$x,y = pp$y,labels="Intrinsic growth rate sp1",adj=c(0,1.5),cex=1.8)
  pp <- s3d$xyz.convert(0,-1.4,0)
  text(x = pp$x,y = pp$y,labels="Intrinsic growth rate sp2",adj=c(0,1.5),cex=1.8,srt=31)
  pp <- s3d$xyz.convert(0,0,0.75)
  text(x = pp$x,y = pp$y,labels="Intrinsic growth rate sp3",adj=c(0,1.5),cex=1.8,srt=90)

  lambda = c(0,1.2)
  X <- v1[1] * lambda 
  Y <- v1[2] * lambda 
  Z <- v1[3] * lambda 
  s3d$points3d(X,-Y,Z,type='l',col='mediumseagreen',lwd=4) 

  X2 <- v2[1] * lambda
  Y2 <- v2[2] * lambda
  Z2 <- v2[3] * lambda
  s3d$points3d(X2,-Y2,Z2,type='l',col='mediumseagreen',lwd=4) 

  X3 <- v3[1] * lambda
  Y3 <- v3[2] * lambda
  Z3 <- v3[3] * lambda
  s3d$points3d(X3,-Y3,Z3,type='l',col='mediumseagreen',lwd=4) 

  X4 <- vc[1] * lambda
  Y4 <- vc[2] * lambda
  Z4 <- vc[3] * lambda
  s3d$points3d(X4,-Y4,Z4,type='l',col='orange3',lwd=4) 


  lambda = c(1.2,10)
  X <- v1[1] * lambda 
  Y <- v1[2] * lambda 
  Z <- v1[3] * lambda 
  s3d$points3d(X,-Y,Z,type='l',col='mediumseagreen',lwd=4,lty=2) 

  X2 <- v2[1] * lambda
  Y2 <- v2[2] * lambda
  Z2 <- v2[3] * lambda
  s3d$points3d(X2,-Y2,Z2,type='l',col='mediumseagreen',lwd=4,lty=2) 

  X3 <- v3[1] * lambda
  Y3 <- v3[2] * lambda
  Z3 <- v3[3] * lambda
  s3d$points3d(X3,-Y3,Z3,type='l',col='mediumseagreen',lwd=4,lty=2) 

  X4 <- vc[1] * lambda
  Y4 <- vc[2] * lambda
  Z4 <- vc[3] * lambda
  s3d$points3d(X4,-Y4,Z4,type='l',col='orange3',lwd=4,lty=2) 

  a <- seq(0,1,by=0.01)
  b <- sqrt(1-a^2)
  c <- rep(0,length(a))
  s3d$points3d(a*1.2,-b*1.2,c*1.2,type='l',col='grey50',lwd=2) 
  s3d$points3d(c*1.2,-a*1.2,b*1.2,type='l',col='grey50',lwd=2) 
  s3d$points3d(b*1.2,-c*1.2,a*1.2,type='l',col='grey50',lwd=2) 


  mu <- seq(0,1,by=0.01)
  w1 <- t(t(v1)) %*% t(mu) + t(t(v2)) %*% t(1-mu)
  w1 <- w1 %*% diag(1/sqrt(colSums(w1^2)))

  w2 <- t(t(v2)) %*% t(mu) + t(t(v3)) %*% t(1-mu)
  w2 <- w2 %*% diag(1/sqrt(colSums(w2^2)))

  w3 <- t(t(v3)) %*% t(mu) + t(t(v1)) %*% t(1-mu)
  w3 <- w3 %*% diag(1/sqrt(colSums(w3^2)))

  wp1 <- s3d$xyz.convert(w1[1,]*1.2,-w1[2,]*1.2,w1[3,]*1.2)
  wp2 <- s3d$xyz.convert(w2[1,]*1.2,-w2[2,]*1.2,w2[3,]*1.2)
  wp3 <- s3d$xyz.convert(w3[1,]*1.2,-w3[2,]*1.2,w3[3,]*1.2)

  XXX <- c(wp1$x, wp2$x, wp3$x)
  YYY <- c(wp1$y, wp2$y, wp3$y)

  color <- col2rgb("mediumseagreen")
  polygon(XXX,YYY,col= rgb(color[1,1],color[2,1],color[3,1],90,maxColorValue=255), border=FALSE)

  s3d$points3d(w1[1,]*1.2,-w1[2,]*1.2,w1[3,]*1.2,type='l',col='mediumseagreen',lwd=4) 
  s3d$points3d(w2[1,]*1.2,-w2[2,]*1.2,w2[3,]*1.2,type='l',col='mediumseagreen',lwd=4) 
  s3d$points3d(w3[1,]*1.2,-w3[2,]*1.2,w3[3,]*1.2,type='l',col='mediumseagreen',lwd=4) 

  dev.off()

}


#######################
projection_3sp <- function(alpha,file_name,sp_name){
  
  graphics.off()
  pdf(file=file_name, width = 8, height = 8, useDingbat=FALSE, family = "ArialMT")
  par(mar = c(0,0,0,0))
  
  ## this is the key part perhaps? Unsure what the ecological interpretation is.
  D <- diag(1/sqrt(diag(t(alpha)%*%alpha)))
  alpha_n <- alpha %*% D
  
  v1 <- alpha_n[,1] ## select first column of alpha_n matrix
  v2 <- alpha_n[,2] ## select 2nd col
  v3 <- alpha_n[,3] ## select 3rd col
  vc <- (v1 + v2 + v3) ## create vector of v1, v2, v3
  vc <- vc / sqrt(sum(vc^2)) ## don't know why this is being done
  
  ## this could be a way of normalizing? Uncertain why they are 
  ## dividing every quantity in the character vector by the sum of the vector elements
  v1 <- v1/sum(v1)
  v2 <- v2/sum(v2)
  v3 <- v3/sum(v3)
  vc <- vc/sum(vc)

  ## setting x limits
  Xf <- sqrt(2); 
  Yf <- sqrt(6)/2;
  
  ## setting y limits
  XX <- c(-Xf/2,Xf/2,0,-Xf/2)
  YY <- c(0,0,Yf,0)
  
  plot(-XX,YY,axes=F,xlab='',ylab='',xlim=c(-Xf/2-0.1,Xf/2+0.1),ylim=c(0-0.1,Yf+0.1),col='grey50',type='l',lwd=2)

  v1C <- c((0.5-0.5*v1[3]-v1[1])*Xf,v1[3]*Yf)
  v2C <- c((0.5-0.5*v2[3]-v2[1])*Xf,v2[3]*Yf)
  v3C <- c((0.5-0.5*v3[3]-v3[1])*Xf,v3[3]*Yf)
  vcC <- c((0.5-0.5*vc[3]-vc[1])*Xf,vc[3]*Yf)
  
  color <- col2rgb("mediumseagreen")
  
  polygon(-c(v1C[1],v2C[1],v3C[1]),c(v1C[2],v2C[2],v3C[2]),col= rgb(color[1,1],color[2,1],color[3,1],90,maxColorValue=255), border=FALSE)
  
  points(-c(v1C[1],v2C[1],v3C[1],v1C[1]) , c(v1C[2],v2C[2],v3C[2],v1C[2]), col= 'mediumseagreen', type='l',cex=1.5,lwd=2)
  points(-c(v1C[1],v2C[1],v3C[1]),c(v1C[2],v2C[2],v3C[2]),col='mediumseagreen',pch=16,cex=1.5*1.27)
  points(-vcC[1],vcC[2],col='orange3',pch=16,cex=1.5*1.27)
  
  text(-XX,YY,labels = sp_name,cex=1.7*1.27,pos = c(1,1,3))
  
  dev.off()
  
}


#######################
projection_4sp <- function(alpha,file_name,sp_name){
  
  graphics.off()
  pdf(file=file_name, width = 8, height = 8, useDingbat=FALSE, family = "ArialMT")
  par(mar = c(0,0,0,0))
  
  D <- diag(1/sqrt(diag(t(alpha)%*%alpha)))
  alpha_n <- alpha %*% D
  
  v1 <- alpha_n[,1]
  v2 <- alpha_n[,2]
  v3 <- alpha_n[,3]
  v4 <- alpha_n[,4]
  vc <- (v1 + v2 + v3 + v4)
  vc <- vc / sqrt(sum(vc^2))
  
  v1 <- v1/sum(v1)
  v2 <- v2/sum(v2)
  v3 <- v3/sum(v3)
  v4 <- v4/sum(v4)
  vc <- vc/sum(vc)
  
  e1 <- c(1,0,0,0)
  e2 <- c(0,1,0,0)
  e3 <- c(0,0,1,0)
  e4 <- c(0,0,0,1)
  
  w2 <- t(t(e2-e1))
  w3 <- t(t(e3-e1))
  w4 <- t(t(e4-e1))
  
  A <- gramSchmidt(cbind(w2,w3,w4))
  
  TT <- A$R
  
  v1 <- v1[-1]
  v2 <- v2[-1]
  v3 <- v3[-1]
  v4 <- v4[-1]
  vc <- vc[-1]
  
  v1 <- as.vector(TT %*% v1)
  v2 <- as.vector(TT %*% v2)
  v3 <- as.vector(TT %*% v3)
  v4 <- as.vector(TT %*% v4)
  vc <- as.vector(TT %*% vc)
  
  
  e1 <- c(0,0,0)
  e2 <- c(1,0,0)
  e3 <- c(0,1,0)
  e4 <- c(0,0,1)
  
  e1 <- as.vector(TT %*% e1)
  e2 <- as.vector(TT %*% e2)
  e3 <- as.vector(TT %*% e3)
  e4 <- as.vector(TT %*% e4)
  
  X <- c(e1[1],e2[1])
  Y <- c(e1[2],e2[2])
  Z <- c(e1[3],e2[3])
  
  s3d <- scatterplot3d(X, Y, Z, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), zlim=c(0,sqrt(2)),type = 'l',
                      box = F, angle = 20, axis = F,color = "grey50", grid=F, lwd=2)
  
  X <- c(e1[1],e3[1])
  Y <- c(e1[2],e3[2])
  Z <- c(e1[3],e3[3])
  s3d$points3d(X,Y,Z,type='l',col='grey50',lwd=2) 
  
  X <- c(e1[1],e4[1])
  Y <- c(e1[2],e4[2])
  Z <- c(e1[3],e4[3])
  s3d$points3d(X,Y,Z,type='l',col='grey50',lwd=2) 
  
  X <- c(e2[1],e3[1])
  Y <- c(e2[2],e3[2])
  Z <- c(e2[3],e3[3])
  s3d$points3d(X,Y,Z,type='l',col='grey50',lwd=2) 
  
  X <- c(e2[1],e4[1])
  Y <- c(e2[2],e4[2])
  Z <- c(e2[3],e4[3])
  s3d$points3d(X,Y,Z,type='l',col='grey50',lwd=2) 
  
  X <- c(e3[1],e4[1])
  Y <- c(e3[2],e4[2])
  Z <- c(e3[3],e4[3])
  s3d$points3d(X,Y,Z,type='l',col='grey50',lwd=2) 
  
  
  color <- col2rgb("mediumseagreen")
  
  f1c <- cbind(v1,v2,v3,v1)
  f1 <- s3d$xyz.convert(f1c[1,],f1c[2,],f1c[3,])
  XXX <- f1$x
  YYY <- f1$y
  polygon(XXX,YYY,col= rgb(color[1,1],color[2,1],color[3,1],90,maxColorValue=255), border=FALSE)
  
  f1c <- cbind(v1,v2,v4,v1)
  f1 <- s3d$xyz.convert(f1c[1,],f1c[2,],f1c[3,])
  XXX <- f1$x
  YYY <- f1$y
  polygon(XXX,YYY,col= rgb(color[1,1],color[2,1],color[3,1],90,maxColorValue=255), border=FALSE)
  
  f1c <- cbind(v1,v3,v4,v1)
  f1 <- s3d$xyz.convert(f1c[1,],f1c[2,],f1c[3,])
  XXX <- f1$x
  YYY <- f1$y
  polygon(XXX,YYY,col= rgb(color[1,1],color[2,1],color[3,1],90,maxColorValue=255), border=FALSE)
  
  f1c <- cbind(v2,v3,v4,v2)
  f1 <- s3d$xyz.convert(f1c[1,],f1c[2,],f1c[3,])
  XXX <- f1$x
  YYY <- f1$y
  polygon(XXX,YYY,col= rgb(color[1,1],color[2,1],color[3,1],90,maxColorValue=255), border=FALSE)
  
  
  X <- c(v1[1],v2[1])
  Y <- c(v1[2],v2[2])
  Z <- c(v1[3],v2[3])
  s3d$points3d(X,Y,Z,type='l',col='mediumseagreen',lwd=2) 
  
  X <- c(v1[1],v3[1])
  Y <- c(v1[2],v3[2])
  Z <- c(v1[3],v3[3])
  s3d$points3d(X,Y,Z,type='l',col='mediumseagreen',lwd=2) 
  
  X <- c(v1[1],v4[1])
  Y <- c(v1[2],v4[2])
  Z <- c(v1[3],v4[3])
  s3d$points3d(X,Y,Z,type='l',col='mediumseagreen',lwd=2) 
  
  X <- c(v2[1],v3[1])
  Y <- c(v2[2],v3[2])
  Z <- c(v2[3],v3[3])
  s3d$points3d(X,Y,Z,type='l',col='mediumseagreen',lwd=2) 
  
  X <- c(v2[1],v4[1])
  Y <- c(v2[2],v4[2])
  Z <- c(v2[3],v4[3])
  s3d$points3d(X,Y,Z,type='l',col='mediumseagreen',lwd=2) 
  
  X <- c(v3[1],v4[1])
  Y <- c(v3[2],v4[2])
  Z <- c(v3[3],v4[3])
  s3d$points3d(X,Y,Z,type='l',col='mediumseagreen',lwd=2) 
  
  X <- v1[1]
  Y <- v1[2]
  Z <- v1[3]
  s3d$points3d(X,Y,Z,col='mediumseagreen',pch=16,cex=1.5*1.27) 
  
  X <- v2[1]
  Y <- v2[2]
  Z <- v2[3]
  s3d$points3d(X,Y,Z,col='mediumseagreen',pch=16,cex=1.5*1.27) 
  
  X <- v3[1]
  Y <- v3[2]
  Z <- v3[3]
  s3d$points3d(X,Y,Z,col='mediumseagreen',pch=16,cex=1.5*1.27) 
  
  X <- v4[1]
  Y <- v4[2]
  Z <- v4[3]
  s3d$points3d(X,Y,Z,col='mediumseagreen',pch=16,cex=1.5*1.27) 
  
  X <- vc[1]
  Y <- vc[2]
  Z <- vc[3]
  s3d$points3d(X,Y,Z,col='orange3',pch=16,cex=1.5*1.27) 
  
  
  wp1 <- s3d$xyz.convert(e1[1],e1[2],e1[3])
  wp2 <- s3d$xyz.convert(e2[1],e2[2],e2[3])
  wp3 <- s3d$xyz.convert(e3[1],e3[2],e3[3])
  wp4 <- s3d$xyz.convert(e4[1],e4[2],e4[3])
  
  XX <- c(wp1$x,wp2$x,wp3$x,wp4$x)
  YY <- c(wp1$y,wp2$y,wp3$y,wp4$y)
  
  text(XX,YY,labels = sp_name,cex=1,pos = c(1,1,4,3))
  
  dev.off()
  
}
  

  
#######################

projection_3sp_with_pairwise <- function(alpha,file_name,sp_name){
  
  graphics.off()
  pdf(file=file_name, width = 8, height = 8, useDingbat=FALSE, family = "ArialMT")
  par(mar = c(0,0,0,0))
  
  D <- diag(1/sqrt(diag(t(alpha)%*%alpha)))
  alpha_n <- alpha %*% D
  
  v1 <- alpha_n[,1]
  v2 <- alpha_n[,2]
  v3 <- alpha_n[,3]
  vc <- (v1 + v2 + v3)
  vc <- vc / sqrt(sum(vc^2))
  
  v1 <- v1/sum(v1)
  v2 <- v2/sum(v2)
  v3 <- v3/sum(v3)
  vc <- vc/sum(vc)
  
  Xf <- sqrt(2);
  Yf <- sqrt(6)/2;
  
  XX <- c(-Xf/2,Xf/2,0,-Xf/2)
  YY <- c(0,0,Yf,0)
  
  plot(-XX,YY,axes=F,xlab='',ylab='',xlim=c(-Xf/2-0.05,Xf/2+0.05),ylim=c(0-0.05,Yf+0.05),col='grey50',type='l',lwd=2)
  
  #####
  
  v1P <- v1
  v1P[3] <- 0
  v1P <- v1P / sum(v1P)
  
  v2P <- v2
  v2P[3] <- 0
  v2P <- v2P / sum(v2P)
  
  vcP <- v1P/sqrt(sum(v1P^2)) + v2P/sqrt(sum(v2P^2))
  vcP[3] <- 0
  vcP <- vcP / sum(vcP)
  
  v1C <- c((0.5-0.5*v1P[3]-v1P[1])*Xf,v1P[3]*Yf)
  v2C <- c((0.5-0.5*v2P[3]-v2P[1])*Xf,v2P[3]*Yf)
  vcC <- c((0.5-0.5*vcP[3]-vcP[1])*Xf,vcP[3]*Yf)
  
  lines(-c(v1C[1],v2C[1]),c(v1C[2],v2C[2]),col='mediumseagreen',lwd=2)
  lines(-c(v1C[1],XX[3]),c(v1C[2],YY[3]),col='mediumseagreen',lty=2,lwd=2)
  lines(-c(v2C[1],XX[3]),c(v2C[2],YY[3]),col='mediumseagreen',lty=2,lwd=2)
  color <- col2rgb("mediumseagreen")
  polygon(-c(v1C[1],v2C[1],XX[3],v1C[1]),c(v1C[2],v2C[2],YY[3],v1C[2]),col=rgb(color[1,1],color[2,1],color[3,1],30,maxColorValue=255) ,border = F  )
  points(-c(v1C[1],v2C[1]),c(v1C[2],v2C[2]),col='dodgerblue',pch=16,cex=1.5)
  #points(-vcC[1],vcC[2],col='orange3',pch=16,cex=1.5)
  
  #####
  
  v1P <- v1
  v1P[2] <- 0
  v1P <- v1P / sum(v1P)
  
  v3P <- v3
  v3P[2] <- 0
  v3P <- v3P / sum(v3P)
  
  vcP <- v1P/sqrt(sum(v1P^2)) + v3P/sqrt(sum(v3P^2))
  vcP[2] <- 0
  vcP <- vcP / sum(vcP)
  
  v1C <- c((0.5-0.5*v1P[3]-v1P[1])*Xf,v1P[3]*Yf)
  v3C <- c((0.5-0.5*v3P[3]-v3P[1])*Xf,v3P[3]*Yf)
  vcC <- c((0.5-0.5*vcP[3]-vcP[1])*Xf,vcP[3]*Yf)
  lines(-c(v1C[1],v3C[1]),c(v1C[2],v3C[2]),col='mediumseagreen',lwd=2)
  lines(-c(v1C[1],XX[2]),c(v1C[2],YY[2]),col='mediumseagreen',lty=2,lwd=2)
  lines(-c(v3C[1],XX[2]),c(v3C[2],YY[2]),col='mediumseagreen',lty=2,lwd=2)
  polygon(-c(v1C[1],v3C[1],XX[2],v1C[1]),c(v1C[2],v3C[2],YY[2],v1C[2]),col=rgb(color[1,1],color[2,1],color[3,1],30,maxColorValue=255) ,border = F  )
  points(-c(v1C[1],v3C[1]),c(v1C[2],v3C[2]),col='dodgerblue',pch=16,cex=1.5)
  #points(-vcC[1],vcC[2],col='orange3',pch=16,cex=1.5)
  
  
  #####
  
  v2P <- v2
  v2P[1] <- 0
  v2P <- v2P / sum(v2P)
  
  v3P <- v3
  v3P[1] <- 0
  v3P <- v3P / sum(v3P)
  
  vcP <- v2P/sqrt(sum(v2P^2)) + v3P/sqrt(sum(v3P^2))
  vcP[1] <- 0
  vcP <- vcP / sum(vcP)
  
  
  v2C <- c((0.5-0.5*v2P[3]-v2P[1])*Xf,v2P[3]*Yf)
  v3C <- c((0.5-0.5*v3P[3]-v3P[1])*Xf,v3P[3]*Yf)
  vcC <- c((0.5-0.5*vcP[3]-vcP[1])*Xf,vcP[3]*Yf)
  
  lines(-c(v2C[1],v3C[1]),c(v2C[2],v3C[2]),col='mediumseagreen',lwd=2)
  lines(-c(v2C[1],XX[1]),c(v2C[2],YY[1]),col='mediumseagreen',lty=2,lwd=2)
  lines(-c(v3C[1],XX[1]),c(v3C[2],YY[1]),col='mediumseagreen',lty=2,lwd=2)
  polygon(-c(v2C[1],v3C[1],XX[1],v2C[1]),c(v2C[2],v3C[2],YY[1],v2C[2]),col=rgb(color[1,1],color[2,1],color[3,1],30,maxColorValue=255) ,border = F  )
  points(-c(v2C[1],v3C[1]),c(v2C[2],v3C[2]),col='dodgerblue',pch=16,cex=1.5)
  
  
  
  v1C <- c((0.5-0.5*v1[3]-v1[1])*Xf,v1[3]*Yf)
  v2C <- c((0.5-0.5*v2[3]-v2[1])*Xf,v2[3]*Yf)
  v3C <- c((0.5-0.5*v3[3]-v3[1])*Xf,v3[3]*Yf)
  vcC <- c((0.5-0.5*vc[3]-vc[1])*Xf,vc[3]*Yf)
  
  color <- col2rgb("green4")
  
  polygon(-c(v1C[1],v2C[1],v3C[1]),c(v1C[2],v2C[2],v3C[2]),col= rgb(color[1,1],color[2,1],color[3,1],90,maxColorValue=255), border=FALSE)
  
  points(-c(v1C[1],v2C[1],v3C[1],v1C[1]) , c(v1C[2],v2C[2],v3C[2],v1C[2]), col= 'green4', type='l',cex=1.5,lwd=2)
  points(-c(v1C[1],v2C[1],v3C[1]),c(v1C[2],v2C[2],v3C[2]),col='blue4',pch=16,cex=1.5)
  
  text(-XX,YY,labels = sp_name,cex=1.7,pos = c(1,1,3))
  
  dev.off()
  
}
