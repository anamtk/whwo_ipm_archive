model{
  #Model description:
  #this is a dynamic occupancy model
  #(https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/06-0669.1)
  #in which values for occupancy in each year are dependent on values 
  #of occupancy in that site the prior year.

  #There is a separate process describing year 1, which is estiamted
  #based on initial occupancy probability (psi1) and all following year
  #occupancy, which is dependent on the previous year's occupancy at that
  #site and the combined effects of persistence (psi) and colonization
  #(gamma). These three probabilities (phi1, psi, and gamma) can be dependent
  #on covariates. 
  
  #observed data is dependent on detection probability (and potentially 
  #detection covariates) and also conditioned on "true" occupancy 
  
  #latent true occuapncy is z in this model.
  
  
  #this model is part 2 of a two-step process for getting posteriors to run 
  #in the IPM model to generate posterior samples that will be used
  #as "data" in the adult portions of the IPM. 
  
  #In this particular model, we have these details:
  #1. a smaller subset of just important 
  ##covariates shaping persistence, psi,
  #derived from FACTS, and various land cover datasets
  #describing conditions at larger (1km) 
  #radius around each point or at all points in a transect
  
  #2. we no longer have covariates to detection because none of 
  ## these demonstrated to be important in the previous modeling step
  
  #Biological process model
  for(i in 1:n.points){
    
    #Initial year values
    z[i,n.start[i]] ~ dbern(psi1)
    
    for(t in (n.start[i]+1):n.end[i]){
      #in subsequent years
      #occupoancy is dependent on persistence, phi
      # and colonization, gamma
      
      #persisetence only applies if previous year z = 1
      # colonization only applies if previous year z =0
      
      logit(phi[i,t-1]) <- 
        a0[ForestID[i]] + 
        a[1]*Tmean_sp[i,t-1] +
        a[2]*Tmean_wt[i, t-1] +
        a[3]*PPT_wt[i,t-1] +
        a[4]*LandHa[i,t-1] +
        a[5]*LowCC[i,t-1] 
      
      #year 2+ occupancy
      z[i, t] ~ dbern(z[i, t-1]*phi[i,t-1] +
                        (1 - z[i, t-1])*gamma[t-1])
    }
    
  }  
  
  #Detection model
  for(i in 1:n.points){ #points
    for(t in n.start[i]:n.end[i]){ #years
        #detection is bionmiaml based on detection probability times
        # true occupancy, with the number of trials being the 
        # nubmer of visits to that site that year
        y.occ[i,t] ~ dbin(p * z[i,t], n.rep[i,t])
        #y.occ - is some number 0 - n.rep[i,t]
      
      #to get log likelihood for hierarchical partitioning
      #https://www.nature.com/articles/s41559-021-01504-1
      #will average this for each model with different 
      #covariate groups.
      ll[i,t] <- logdensity.bin(y.occ[i,t], p*z[i,t], n.rep[i,t])
    } #year
  } #point
      
  #PRIORS
  
  #regression of adult survival priors
  for(f in 1:n.forests){
  a0[f] ~ dnorm(0, 1E-2)
  }

  for(i in 1:5){
    a[i] ~ dnorm(0, 1E-2)
  }
  
  psi1 ~ dbeta(1, 1)

  #detection prior
  p ~ dbeta(1,1)

  #getting gamma priors for gains rate parameter
  for(t in 1:(n.years-1)){
    gamma[t] ~ dbeta(1, 1)
  }

  
  }
  
