#simplified occupancy model for getting posteriors to run in IPM
model{
  #Occupancy model
  for(i in 1:n.points){
    #Initial year values
    
    # logit(psi1[i]) <- 
    #   b0[ForestID[i]] + 
    #   b[1]*Tmean_sp[i, n.start[i]] +
    #   b[2]*PPT_sp[i,n.start[i]] +
    #   b[3]*SHDI[i,n.start[i]] + 
    #   b[4]*LandHa[i, n.start[i]] +
    #   b[5]*LowCC[i, n.start[i]] +
    #   b[6]*MedCC[i, n.start[i]]
    
    z[i,n.start[i]] ~ dbern(psi1)
    
    for(t in (n.start[i]+1):n.end[i]){
      #in subsequent years
      #occupoancy is dependent on persistence, phi
      # and colonization, gamma
      
      #persisetence only applies if previous year z = 1
      # colonization only applies if previous year z =0
      
      logit(phi[i,t-1]) <- 
        a0[ForestID[i]] + 
        a[1]*LandHa[i,t-1] 
      
      #Tmean_wt, PPT_sp, PPT_wt, LandHa, HighCC
      
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
      
      ll[i,t] <- logdensity.bin(y.occ[i,t], p*z[i,t], n.rep[i,t])
      
      
    } #year
  } #point
      
  #PRIORS
  
  #regression of adult survival priors
  for(f in 1:n.forests){
  a0[f] ~ dnorm(0, 1E-2)
  #b0[f] ~ dnorm(0, 1E-2)
  }

  for(i in 1:1){
    a[i] ~ dnorm(0, 1E-2)
  }
  
  psi1 ~ dbeta(1, 1)
  
  # for(i in 1:6){
  #   b[i] ~ dnorm(0, 1E-2)
  # }
  
  #detection prior
  p ~ dbeta(1,1)

  #getting gamma priors for gains rate parameter
  for(t in 1:(n.years-1)){
    gamma[t] ~ dbeta(1, 1)
  }

  
  }
  
