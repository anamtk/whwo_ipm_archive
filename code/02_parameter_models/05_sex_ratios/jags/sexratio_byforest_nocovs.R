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
    #we're estimating separately for each forest population
     y[i] ~ dbin(propF[Forest.ID[i]], n.total[i])
    
  }

  #PRIORS
  #logit transform since poisson rate parameter has to be >0,
  #this allows egg.lambda to be on the entire
  #real line
  for(f in 1:n.forests){
    logit.propF[f] ~ dnorm(0, 1E-2)
    propF[f] <- ilogit(logit.propF[f])
  }
  
}