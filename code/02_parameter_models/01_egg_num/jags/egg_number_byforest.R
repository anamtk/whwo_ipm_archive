model{
  #Model description
  #this is a simplified version of a published model 
  #https://www.sciencedirect.com/science/article/pii/S0378112723006771?via%3Dihub
  #examining the relationships between egg number for whwo and
  #various environmental drivers. None of these drivers were 
  #important, so this model does not include covariates 
  #for egg number. 
  
  #We have added one detail not present in the published model, which
  #is to derive egg numbers separately for each forest population so
  #that any regional differences in egg production can be part of the 
  #population model
  
  #this model is used to generate posterior samples of egg.lambda
  #for each forest to be fed into the IPM.
  
  ## Egg number model
  
  for(i in 1:n.nests){
      
    #egg data is poisson distributed around the egg
    #lambda rate parameter
    y.egg[i] ~ dpois(egg.lambda[Forest.ID[i]])
    
  }
  
  #PRIORS
  #log transform since poisson rate parameter has to be >0,
  #this allows egg.lambda to be on the entire
  #real line
  for(f in 1:n.forests){
    logegg.lambda[f] ~ dunif(-5,5)
    egg.lambda[f] <- exp(logegg.lambda[f])
    }
  
}