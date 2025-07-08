model{
  
  #this model explores drivers of sex ratios for 
  #white-headed woodpeckers
  
  #looking at proportion female out of total number of nestlings
  #in every nest
  
  for(i in 1:n.nests){
  
    #number of females, y, is based on
    #probability of being a female for nest i 
    #based on the total number of "trials", in this
    #case, total number of nestlings in that nest
     y[i] ~ dbin(p[i], n.total[i])
    
    
    logit(p[i]) <- f0[Forest.ID[i]] + 
      #larger (often males) tend to survive better in larger broods
      #find citation - but saw in a literature search
      f[1]*BroodSize[i] +
      #some evidence that nest timing impacts sex ratio of
      #fledglings:
      #https://doi.org/10.5253/arde.v109i1.a8
      f[2]*InitDay[i] +
      eps.transect[Transect.ID[i]]
  
  
  }
  
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
    f0[f] ~ dnorm(0, 1E-2)
  }
 
  for(i in 1:2){
    f[i] ~ dnorm(0, 1E-2)
  }
  
}