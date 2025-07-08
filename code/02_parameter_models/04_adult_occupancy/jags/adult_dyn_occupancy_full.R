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
  
  #this model part 1 of a two-step process in which we first used this
  #model to determine which covariates were important for both 
  #detection and persistence (no covariates on phi1 or gamma) and then 
  #used these in a simplified model to estimate covariate effects in a 
  #population model. 
  
  #In this particular model, we have these details:
  #1. covariates shaping persistence, psi,
  #derived from FACTS, and various land cover datasets
  #describing conditions at somewhat local (5m) and larger (1km) 
  #radius around each point or at all points in a transect
  
  #2. effects of survey lenght ('effort') and observer id random
  ## effects on detection probability. 
  
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
      
      #could phi approaximate adult survival?
      logit(phi[i,t-1]) <- 
        a0 + 
        #a1TrtID[TreatmentID[i, t-1]] +
        a[2]*Tmean_sp[i,t-1] +
        a[3]*Tmean_wt[i,t-1] +
        a[4]*PPT_sp[i,t-1] +
        a[5]*PPT_wt[i,t-1] +
        #a[6]*SHDI[i,t-1] + #removed bc correlated with LandHa
        a[6]*LandBu[i,t-1] +
        a[7]*LandHa[i,t-1] +
        a[8]*LowCC[i,t-1] +
        #a[10]*MedCC[i, t-1] + #removed bc correlated with low and High CC
        a[9]*HighCC[i, t-1] 
      
      #year 2+ occupancy
      z[i, t] ~ dbern(z[i, t-1]*phi[i,t-1] +
                        (1 - z[i, t-1])*gamma[t-1])
    }
  }  
  
  #Detection model
  for(i in 1:n.points){ #points
    for(t in n.start[i]:n.end[i]){ #years
      for(j in 1:n.rep[i,t]){ #number of times each point is visited/year

        #depetection probability p is based on covariates
        logit(p[i,j,t]) <- e0 + 
          e[1]*effort[i,j,t] +
          eps.observer[Observer[i,j,t]]
        #detection is bionmiaml based on detection probability times
        # true occupancy, with the number of trials being the 
        # nubmer of visits to that site that year
        y.occ[i,j,t] ~ dbern(p[i,j,t] * z[i,t])
        
        #replicated data for Goodness of fit
        y.rep[i,j,t] ~ dbern(p[i,j,t] * z[i,t])
        
      } #rep
    } #point
  } #year
  
  #missing data
  for(i in 1:n.points){ #points
    for(t in n.start[i]:n.end[i]){ #years
      for(j in 1:n.rep[i,t]){ #number of times each point is visited/year
        effort[i,j,t] ~ dnorm(mu.eff, tau.eff)
        Observer[i,j,t] ~ dcat(mu.pi)
      } #rep
    }
  }
      
  #PRIORS
  #regression of adult survival priors
  a0 ~ dnorm(0, 1E-2)
  
  # for(t in 2:n.trt){
  #   a1TrtID[t]~ dnorm(0, 1E-2)
  # }
  # a1TrtID[1] <- 0
  
  for(i in 2:9){
    a[i] ~ dnorm(0, 1E-2)
    
  }
  
  psi1 ~ dbeta(1, 1)

  #regression of detection priors
  e0 ~ dnorm(0, 1E-2)
  
  for(i in 1:1){
    e[i] ~ dnorm(0, 1E-2)
  } 
  
  #Sum-to-zero method for observer effects
  #(for identifiability)
  #for every observer but the last one:
  for(t in 1:(n.observer-1)){
    eps.observer[t] ~ dnorm(0, tau.observer)
  }
  #set the last observer to be the -sum of all other 
  # observers  so the 
  # overall fo all observer levels == 0
  eps.observer[n.observer] <- -sum(eps.observer[1:(n.observer-1)])
  
  #obsrever-level variation
  sig.observer ~ dunif(0,10)
  tau.observer <- 1/pow(sig.observer,2)
  
  #getting gamma priors for gains rate parameter
  for(t in 1:(n.years-1)){
    gamma[t] ~ dbeta(1, 1)
  }
  
  #Missing data priors
  mu.eff ~ dunif(-10, 10)
  sig.eff ~ dunif(0, 20)
  tau.eff <- pow(sig.eff, -2)
  
  for(p in 1:n.observer){#each pi for each integer gets equal probability of being picked
    mu.pi[p] <- 1/n.observer #each has a probability that is equal if we divide by total
  }
  
  #-------------------------------------## 
  # Covariate P-values ###
  #-------------------------------------##
  
  #generate a 1-0 vector for each covariate
  #such that 1 = + in that iteration, 0 = - in that iteration
  # the mean of this value will tell us whether something is mostly positive
  # (high mean posterior value), mostly negative (low mean posterior value)
  # or somewhree in the middle (often 0, so 0.5 mean posterior)
  
  #generates per level of categorical variables
  #z.a1 <- step(a1TrtID)
  
  #generate p-values for all continuous covariates
  for(i in 2:9){
    z.a[i] <- step(a[i])
  }
  
  }
  
