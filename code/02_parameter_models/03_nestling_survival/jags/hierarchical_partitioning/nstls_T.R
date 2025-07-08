model{
    
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
        n[3]*LandBu[i] +
        eps.transect[Transect.ID[i]]
      
      # #GOODNESS OF FIT OBJECTS
      # yrep[i] ~ dbin(psi.nestling[i], Nnestlings[i])
      # 
      # resid[i] <- y.fledgling[i] - psi.nestling[i]
      # 
      # #MODEL SELECTION
      # sqdiff[i] <- (yrep[i] - y.fledgling[i])^2
      
      #Log Likelihood for doing hierarchical partitioning
      ll[i] <- logdensity.bin(y.fledgling[i], psi.nestling[i], Nnestlings[i])
      

      #   
    }
  
  # #MODEL SELECTION
  # Dsum <- sum(sqdiff[])
  
  #mean of the log likelihoods to get a model-level one
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
  
  for(i in 3){
    n[i] ~ dnorm(0, 1E-2)
  }


  
}
