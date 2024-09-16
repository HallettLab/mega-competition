##list of functions :

#one network metric function ####
#diagonal dominance function
dominance <- function(alpha){
  dom<-NA
  for (i in 1:nrow(alpha)){
    ii<-alpha[row(alpha) == col(alpha)]
    diag(alpha)<-0
    ij<-alpha[i,]
    ij<-ij[ij!=0]
    d <- abs(ii[i]) - (sum(abs(ij)))
    dom<-rbind(dom,d)
  }
  dom<-dom[-1,]
  dom<-mean(dom)
  return(dom)
}


#remove combinations of species that are identical in two data frames ####
removeidentical<- function(alphai,beta){
  for (i in 1:ncol(alphai)) {
    m<-alphai[,i]
    for (j in 1:ncol(beta)) {
      n<-beta[,j]
      if (identical(m,n)){
        alphai[,i]<-NA
        i=i+1
      }
    }
  }
  alphai <- alphai[,colSums(is.na(alphai))<nrow(alphai)]
  colnames(alphai)<-NULL
  return(alphai)
}


# get all network metrics, formatted in a good way: all=competition matrix, dat= all combinations of species in the network ####
networkmetrics<-function(dat,rich){
  species<-data.frame(matrix(nrow=0,ncol = length(sp)))
  colnames(species)<-sp
  mixresults<-data.frame(Modularity=NA,
                         Skewness=NA,
                         Gini=NA,
                         Asymmetry=NA,
                         Diagstrength=NA,
                         SLA=NA,
                         SLAv=NA, 
                         Treatment = NA)
  allresults<-data.frame(Modularity=NA,
                         Skewness=NA,
                         Gini=NA,
                         Asymmetry=NA,
                         Diagstrength=NA,
                         SLA=NA,
                         SLAv=NA, 
                         Treatment = NA)
  
  
  for (j in 1:length(all)) {
    m<-all[[j]] # select one treatment / season
    for (i in 1:ncol(dat)) {
      sps<-dat[,i]
      species[i,]<-ifelse(colnames(species) %in% sps,1,0)
      subalpha<-m[sps,sps] # creates a sub matrix only with target species
      weight<-c(t(subalpha)) # saves the interaction coefficient in a variable
      sp2<-rep(sps,lenght.out = length(weight)) 
      sp1<-rep(sps,each=rich) 
      net<-data.frame(sp1,sp2) # creates every combination of species pair
      g<-graph_from_data_frame(net) # builds a network with all possible paths
      E(g)$weight<-weight # assigns interaction coefficient as weights of paths
      clu<-cluster_optimal(g,weights = abs(E(g)$weight)) # defines which are the best clusters
      mod<-modularity(g,membership(clu), weights = abs(E(g)$weight)) # calculates modularity with absolute values of interaction coefficients as weight
      mixresults[i,1]<-mod # modularity is saved in results
      mixresults[i,2]<-skewness(c(subalpha)) # skewness is saved in results
      v<-c(subalpha) # saves all interaction coefficient to look at their distribution
      mixresults[i,3]<-Gini_RSV(y=v, w=rep(1, length(v))) # calculates the evenness
      asymmetry<-abs(subalpha-t(subalpha)) # absolute difference between upper and lower triangle (competition effect / response)
      diag(asymmetry)<-NA # only keep one triangle
      asymmetry<-lower.tri.remove(asymmetry) # only keep one triangle
      mixresults[i,4]<-mean(c(asymmetry), na.rm = T) # save the asymmetry value in results
      diag<-dominance(subalpha) # assess diagonal dominance (see dominance function in this file)
      mixresults[i,5]<-diag # saves diagonal dominance in results
      mixresults[i,8]<-names(all)[j] # saves the treatment
      meansla<-meansla2019[meansla2019$Species %in% sps,] # selects only the log(SLA) of species in the sub matrix
      mixresults[i,6]<- mean(meansla$value) # saves the mean log(SLA) of the community
      mixresults[i,7]<- var(meansla$value) # saves the variance of log(SLA) in the community
    }
    allresults<-rbind(allresults,mixresults)
  }
  # better formatting
  allresults<-allresults[-1,]
  allresults<-allresults %>% separate(Treatment, c('Season', 'Treatment'))
  allresults$Treatment<-as.factor(allresults$Treatment)
  allresults$Treatment<- factor(allresults$Treatment, levels=c("C", "N", "F", "NF"))
  allresults$Season<-as.factor(allresults$Season)
  allresults$Season<- factor(allresults$Season, levels=c("June", "August"))
  return(list(allresults,species))
}

# function to plot networks and their modules depending on treatment, returns a list of igraph objects and cluster membership for each species in the network ####

networkplot <- function(dat, rich){
  netplot<-list()
  species<-data.frame(matrix(nrow=0,ncol = length(sp)))
  colnames(species)<-sp
  for (j in 1:length(all)) {
    m<-all[[j]] # select one treatment / season
    for (i in 1:ncol(dat)) {
      sps<-dat[,i]
      species[i,]<-ifelse(colnames(species) %in% sps,1,0)
      subalpha<-m[sps,sps] # creates a sub matrix only with target species
      weight<-c(t(subalpha)) # saves the interaction coefficient in a variable
      sp2<-rep(sps,lenght.out = length(weight)) 
      sp1<-rep(sps,each=rich) 
      net<-data.frame(sp1,sp2) # creates every combination of species pair
      g<-graph_from_data_frame(net) # builds a network with all possible paths
      E(g)$weight<-weight # assigns interaction coefficient as weights of paths
      clu<-cluster_optimal(g,weights = abs(E(g)$weight)) # defines which are the best clusters
      mod<-modularity(g,membership(clu), weights = abs(E(g)$weight))
      x<-list(g,clu)
      names(x)<-names(all)[j]
    }
    netplot<-append(netplot,x)
    
  }
  return(netplot)
}


# get network metrics for different levels of sp richness ####
networkrich<-function(n){
  allrich<-NA
  for (i in 1:length(n)){
    rich<-n[[i]]
    mixdat<-as.data.frame(combn(sp,rich))
    # computing all network metrics for all functional communities
    all<-list(alpha_june_control,alpha_june_nitrogen,alpha_june_fungicide,alpha_june_combined,alpha_august_control,alpha_august_nitrogen,alpha_august_fungicide,alpha_august_combined)
    names(all)<-c("June C", "June N", "June F", "June NF", "August C", "August N", "August F", "August NF")
    
    mixnet<-networkmetrics(mixdat,rich=rich)
    speciesmixed<-mixnet[[2]]
    mixnet<-mixnet[[1]]
    mixnet$Communities<-"All"
    mixnet$Richness<-rich
    speciesmat<-speciesmixed[rep(rownames(speciesmixed),8),]
    rownames(speciesmat)<-NULL
    mixnet<-cbind(mixnet,speciesmat)
    allrich<-rbind(allrich,mixnet)
  }
  allrich<-allrich[-1,]
  return(allrich)
}



# prediction for SLA, model is the model used for prediction, comp is the original data set containing Nitrogen, Fungicide, SLA and Season
# returns a fit for the chosen network metrics with confidence intervals
slapred<- function(model, comp){
  
  # creating an empty dataset called newdata
  newdata<-NULL
  
  # initialising the loop with empty result, SLA and SLAv
  sumboot<-NULL
  SLA<-NULL
  SLAv<-NULL
  
  # create new data to predict on a new interval
  # Fake SLA from min to max of our comp original data, 100 datapoints simulated
  SLA<-seq(min(comp$SLA), max(comp$SLA), length.out=100)
  
  # Here is a nice function that compute all possible combination of treatment, note that I set the variance of SLA to 0 (SLAv) in order to decouple SLA from SLAv, I just want to predict SLA.
  newdata <- expand.grid("SLA"=SLA, "Nitrogen" = c(0,1), "Fungicide" = c(0,1), "SLAv" = 0, "Season" = c("June","August"))
  
  # little corrections for 
  newdata$Nitrogen<-as.factor(newdata$Nitrogen)
  newdata$Fungicide<-as.factor(newdata$Fungicide)
  newdata$Treatment <- as.factor(paste(newdata$Nitrogen,newdata$Fungicide))
  
  # This is my fake random factor, so that my predict function works, but note that I don't compute my prediction and confidence intervals with random factors so they are not used for predictions. It is important that they have the same number of levels than my initial data though (18 for 18 species in my phytometers).
  newSpecies <- rep(LETTERS[1:18], length.out= nrow(newdata))
  newdata$Species<-newSpecies
  
  # re ordering my treatment levels -> that's only for good plotting
  levels(newdata$Treatment)<-c("Control","Fungicide","Nitrogen","Combined")
  newdata$Treatment<-factor(newdata$Treatment, c("Control","Nitrogen","Fungicide","Combined"))
  
  
  # Here is the important part : I create a new function, pfun, that use predict() function but doesn't use random factor to predict (re.form = ~0). I need to run it each time in the loop because otherwise it's mixed with my other function predicting SLA variance (or SLAv) instead of SLA.
  pfun <- function(.) {
    predict(., newdata=newdata, re.form=~0, type="response")
  }
  
  # Here is the key function I use, bootMer() from lme4 package, to bootstrap my prediction using my pfun function
  bootdata<-lme4::bootMer(model, pfun, nsim=100, type="parametric")
  
  # Here is the extra function below to summarise bootMer outputs (source : https://deanmarchiori.rbind.io/post/prediction-intervals-for-linear-mixed-effects-models/)
  sumboot<-sumBoot(bootdata)
  
  # saving results in results, makes sense I guess
  results<-cbind(newdata,sumboot)
  
  return(results)
  
}



#prediction for SLA variance, model is the model used for prediction, comp is the original data set containing Nitrogen, Fungicide, SLA variance and Season
#returns a fit for the chosen network metrics with confidence intervals
# the structure and explanations are the same as for the slapred function

slavpred<- function(model, comp){
  
  newdata<-NULL
  sumboot<-NULL
  SLA<-NULL
  SLAv<-NULL
  #create new data to predict on a new interval
  SLAv<-seq(min(comp$SLAv), max(comp$SLAv), length.out=100)
  newdata <- expand.grid("SLAv"=SLAv, "Nitrogen" = c(0,1), "Fungicide" = c(0,1), "SLA" = 0, "Season" = c("June","August"))
  newdata$Nitrogen<-as.factor(newdata$Nitrogen)
  newdata$Fungicide<-as.factor(newdata$Fungicide)
  newdata$Treatment <- as.factor(paste(newdata$Nitrogen,newdata$Fungicide))
  newSpecies <- rep(LETTERS[1:18], length.out= nrow(newdata))
  newdata$Species<-newSpecies
  levels(newdata$Treatment)<-c("Control","Fungicide","Nitrogen","Combined")
  newdata$Treatment<-factor(newdata$Treatment, c("Control","Nitrogen","Fungicide","Combined"))
  
  pfun <- function(.) {
    predict(., newdata=newdata, re.form=~0, type="response")
  }
  bootdata<-lme4::bootMer(model, pfun, nsim=100, type="parametric")
  sumboot<-sumBoot(bootdata)
  
  vresults<-cbind(newdata,sumboot)
  
  return(vresults)
}



# prediction for SLA, model is the model used for prediction, comp is the original data set containing Nitrogen, Fungicide, SLA, Season and different levels of network size
# returns a fit for the chosen network metrics with confidence intervals
slarichpred<- function(model, comp, n){
  
  # creating an empty dataset called newdata
  newdata<-list()
  
  # initialising the loop with empty result, SLA and SLAv
  sumboot<-NULL
  SLA<-NULL
  SLAv<-NULL
  
  # create new data to predict on a new interval
  # Fake SLA from min to max of our comp original data, 100 datapoints simulated
  for (i in 1:length(n)){
    
    sub<-subset(comp, comp$network.size==n[[i]])
    SLA<-seq(min(sub$SLA), max(sub$SLA), length.out=50)
    
    # Here is a nice function that compute all possible combination of treatment, note that I set the variance of SLA to 0 (SLAv) in order to decouple SLA from SLAv, I just want to predict SLA.
    newdata[[i]] <- expand.grid("SLA"=SLA, "Nitrogen" = c(0,1), "Fungicide" = c(0,1), "SLAv" = 0, "Season" = c("June","August"),"network.size" = n[[i]])
    
  }
  
  newdata <- melt(newdata, id= c("SLA","Nitrogen","Fungicide","SLAv","Season","network.size"))
  newdata$L1<-NULL
  
  # little corrections for 
  newdata$Nitrogen<-as.factor(newdata$Nitrogen)
  newdata$Fungicide<-as.factor(newdata$Fungicide)
  newdata$Treatment <- as.factor(paste(newdata$Nitrogen,newdata$Fungicide))
  #make an ordered factor out of the richness
  newdata$network.size<-as.factor(as.numeric(newdata$network.size))
  levels(newdata$network.size)<-c(5,7,11,15)
  newdata$network.size<-ordered(newdata$network.size)
  
  # This is my fake random factor, so that my predict function works, but note that I don't compute my prediction and confidence intervals with random factors so they are not used for predictions. It is important that they have the same number of levels than my initial data though (18 for 18 species in my phytometers).
  newSpecies <- rep(LETTERS[1:18], length.out= nrow(newdata))
  newdata$Species<-newSpecies
  
  # re ordering my treatment levels -> that's only for good plotting
  levels(newdata$Treatment)<-c("Control","Fungicide","Nitrogen","Combined")
  newdata$Treatment<-factor(newdata$Treatment, c("Control","Nitrogen","Fungicide","Combined"))
  
  
  # Here is the important part : I create a new function, pfun, that use predict() function but doesn't use random factor to predict (re.form = ~0). I need to run it each time in the loop because otherwise it's mixed with my other function predicting SLA variance (or SLAv) instead of SLA.
  pfun <- function(.) {
    predict(., newdata=newdata, re.form=~0, type="response")
  }
  
  # Here is the key function I use, bootMer() from lme4 package, to bootstrap my prediction using my pfun function
  bootdata<-lme4::bootMer(model, pfun, nsim=100, type="parametric")
  
  # Here is the extra function below to summarise bootMer outputs (source : https://deanmarchiori.rbind.io/post/prediction-intervals-for-linear-mixed-effects-models/)
  sumboot<-sumBoot(bootdata)
  
  # saving results in results, makes sense I guess
  results<-cbind(newdata,sumboot)
  
  return(results)
  
}



#prediction for SLA variance, model is the model used for prediction, comp is the original data set containing Nitrogen, Fungicide, SLA variance, Season and different levels of network size
#returns a fit for the chosen network metrics with confidence intervals
# the structure and explanations are the same as for the slapred function

slarichvpred<- function(model, comp, n){
  
  newdata<-list()
  sumboot<-NULL
  SLA<-NULL
  SLAv<-NULL
  #create new data to predict on a new interval
  for (i in 1:length(n)){
    
    sub<-subset(comp, comp$network.size==n[[i]])
    SLAv<-seq(min(sub$SLAv), max(sub$SLAv), length.out=50)
    newdata[[i]] <- expand.grid("SLAv"=SLAv, "Nitrogen" = c(0,1), "Fungicide" = c(0,1), "SLA" = 0, "Season" = c("June","August"), "network.size" = n[[i]])
    
  }
  newdata <- melt(newdata, id= c("SLA","Nitrogen","Fungicide","SLAv","Season","network.size"))
  newdata$L1<-NULL
  
  newdata$Nitrogen<-as.factor(newdata$Nitrogen)
  newdata$Fungicide<-as.factor(newdata$Fungicide)
  newdata$Treatment <- as.factor(paste(newdata$Nitrogen,newdata$Fungicide))
  #make an ordered factor out of the richness
  newdata$network.size<-as.factor(as.numeric(newdata$network.size))
  levels(newdata$network.size)<-c(5,7,11,15)
  newdata$network.size<-ordered(newdata$network.size)
  newSpecies <- rep(LETTERS[1:18], length.out= nrow(newdata))
  newdata$Species<-newSpecies
  levels(newdata$Treatment)<-c("Control","Fungicide","Nitrogen","Combined")
  newdata$Treatment<-factor(newdata$Treatment, c("Control","Nitrogen","Fungicide","Combined"))
  
  pfun <- function(.) {
    predict(., newdata=newdata, re.form=~0, type="response")
  }
  bootdata<-lme4::bootMer(model, pfun, nsim=100, type="parametric")
  sumboot<-sumBoot(bootdata)
  
  vresults<-cbind(newdata,sumboot)
  
  return(vresults)
}





#function useful for predicitons ####

# summarise output of bootstrapping
sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
               lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
               upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
    )
  )
}