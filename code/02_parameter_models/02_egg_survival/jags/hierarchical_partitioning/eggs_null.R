model{
  
  for(i in 1:n.nests){
      
    #nestling data is binomial distributed around
    #egg survival and the number of "trials" or the
    #number of eggs in the nest
    y.nestling[i] ~ dbin(psi.egg[i], Neggs[i])
      
    #Likelihood for psi.egg is a regression with
    #an intercept and slope parameters for each covarate,
    #and a random effect for which transect the nest is located
    #on
    logit(psi.egg[i]) <- 
      e0[Forest.ID[i]] + 
      eps.transect[Transect.ID[i]]
    
    #Missing temp data
    #making temperature and dependent on forest location,
    #since they have slightly different climates
    #Tmax[i] ~ dnorm(mu.tmax[Forest.ID[i]], tau.tmax[Forest.ID[i]])
    
      
    #Log Likelihood for doing hierarchical partitioning
    ll[i] <- logdensity.bin(y.nestling[i], psi.egg[i], Neggs[i])

  }
  
  #mean of the log likelihoods to get a model-level one
  LL <- sum(ll[])

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
  e0[f] ~ dnorm(0, 1E-2)
  }
  
  # for(t in 2:n.trt){
  # e1TreatmentID[t] ~ dnorm(0, 1E-2)
  # }
  
  #e1TreatmentID[1] <- 0
  
  # for(i in 2:5){
  #   e[i] ~ dnorm(0, 1E-2)
  # }
  
  #these need to be indexed by forest ID
  # for(f in 1:n.forests){
  #   mu.tmax[f] ~ dunif(-10, 10)
  #   sig.tmax[f] ~ dunif(0, 20)
  #   tau.tmax[f] <- pow(sig.tmax[f], -2)
  # }
}