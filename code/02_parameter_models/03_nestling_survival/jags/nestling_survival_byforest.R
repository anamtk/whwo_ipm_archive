model{
  #Model description
  #this is a simplified version of a published model 
  #https://www.sciencedirect.com/science/article/pii/S0378112723006771?via%3Dihub
  #examining the relationships between nestilng survival for whwo and
  #various environmental drivers. This model only includes those
  #covariates shown to be important to nestling survival from that 
  #published model
  
  #We have added one detail not present in the published model, which
  #is to derive baseline nestling surival(intercept) values 
  #separately for each forest population so
  #that any regional differences in nestling survival can be part of the 
  #population model
  
  #this model is used to generate posterior samples of 
  #covariate effects and forest-level intercept values
  #for each forest to be fed into the IPM.
  
    for(i in 1:n.nests){
      #Fledgling data is binomial distributed around
      #nestling survival and the number of "trials" or
      #the number of nestlings in the nest
        y.fledgling[i] ~ dbin(psi.nestling[i], Nnestlings[i])
        
      #Likelihood for psi.nestling is a regression with intercept,
      #slope parameters for every covariate, and random error
      #for which transect the nest is located on
      #for this model, intercept and slope terms depend on forest
      #location
      logit(psi.nestling[i]) <- 
        n0[Forest.ID[i]] + 
        n1TreatmentID[TreatmentID[i]] +
        n2SpeciesID[SpeciesID[i]] +
        n[3]*LandBu[i] +
        n[4]*InitDay[i] +
        n[5]*NestHt[i] +
        n[6]*PPT[i] +
        n[7]*Tmax[i] +
        n[8]*Tmax[i]^2 +
        eps.transect[Transect.ID[i]]
      
      InitDay[i] ~ dnorm(mu.init, tau.init)
      
      #making temperature and dependent on forest location,
      #since they have slightly different climates
      Tmax[i] ~ dnorm(mu.tmax[Forest.ID[i]], tau.tmax[Forest.ID[i]])
      
      #Log Likelihood for doing hierarchical partitioning
      #https://www.nature.com/articles/s41559-021-01504-1
      ll[i] <- logdensity.bin(y.fledgling[i], psi.nestling[i], Nnestlings[i])
      
      
      #   
    }

  #mean of the log likelihoods to get a model-level 
  #Log Likelihood for doing hierarchical partitioning
  #https://www.nature.com/articles/s41559-021-01504-1
  LL <- sum(ll[])
  
  ##PRIORS
  #Sum-to-zero method for transect effects
  #(for identifiability)
  #for every transect but the last one:
  for(t in 1:(n.transects-1)){
    eps.transect[t] ~ dnorm(0, tau.transect)
  }
  #set the last transect to be the -sum of all other transects so the 
  # overall fo all transect levels == 0
  eps.transect[n.transects] <- -sum(eps.transect[1:(n.transects-1)])
  
  #transect-level variation
  sig.transect ~ dunif(0,10)
  tau.transect <- 1/pow(sig.transect,2)

  #Intercept and slope priors for each covariate
  for(f in 1:n.forests){
  n0[f] ~ dnorm(0, 1E-2)
  }
  
  for(t in 2:n.trt){
    n1TreatmentID[t] ~ dnorm(0, 1E-2)
  }
  n1TreatmentID[1] <- 0
  
  for(s in 2:n.species){
    n2SpeciesID[s] ~ dnorm(0, 1E-2) 
  }
  
  n2SpeciesID[1] <- 0
  
  for(i in 3:8){
    n[i] ~ dnorm(0, 1E-2)
  }

  #DATA IMPUTING PRIORS
  #Priors for mean and tau of missing covariates in the model
  mu.init ~ dunif(-10, 10)
  sig.init ~ dunif(0, 20)
  tau.init  <- pow(sig.init, -2)
  
  #these need to be indexed by forest ID
  for(f in 1:n.forests){
    mu.tmax[f] ~ dunif(-10, 10)
    sig.tmax[f] ~ dunif(0, 20)
    tau.tmax[f] <- pow(sig.tmax[f], -2)
  }
  
}
