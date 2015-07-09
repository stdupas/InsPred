 
conquadraticSkewed1 <- function(x, Xmin, Xmax, Xopt, Yopt)
{
  # Asymetric concave conquadratic function within an enveloppe, else returns 0.
  #
  # Args:
  #   x : numeric providing the values of variable to calculate reaction norm
  #   Xmin: start of the enveloppe
  #   Xmax: end of the enveloppe
  #   Xopt: the value that maximises the function
  #   Yopt: the maximum value of the function
  #
  # Returns:
  #   The value of reaction norm
  if(Xopt>=Xmax) Xopt=Xmax-Xmax/100000
  if(Xopt<=Xmin) Xopt=Xmin+Xmin/100000
  a1=(Xmin-Xmax)/(2*(Xmin-Xopt))
  a2=(Xmax-Xmin)/(2*(Xmax-Xopt))
  Xprime<- (x<Xopt)*(a1*x+Xmin*(1-a1))+(x>=Xopt)*(a2*x+Xmax*(1-a2))
  y <- -4*Yopt/(Xmax-Xmin)^2*(Xprime-Xmax)*(Xprime-Xmin)*(x<=Xmax)*(x>=Xmin)
  return(y)
}


developmentRateLogan <- function(T,species,life_stage)
{
  parameters <- switch (species,
                        Bf = switch (life_stage,
                                     Egg = c(Y=0.0093744,Tmax=31.8519816,rho=0.1165808,V=2.3537414),
                                     phyloLarvae = c(Y=3*0.001,Tmax=31.229,rho=0.116,V=1.654),
                                     stemLarvae = c(Y=1.5*0.001,Tmax=31.229,rho=0.116,V=1.654),
                                     Pupae = c(Y=0.006,Tmax=33.039,rho=0.164,V=5.227)
                        ),
                        Sc = switch (life_stage,
                                     Egg = c(Y=0.010,Tmax=31.93,rho=0.110,V=1.786),
                                     phyloLarvae = c(Y=3*0.002,Tmax=35.79,rho=0.150,V=5.695),
                                     stemLarvae = c(Y=1.5*0.002,Tmax=35.79,rho=0.150,V=5.695),
                                     Pupae = c(Y=0.009,Tmax=35.09,rho=0.170,V=5.427)
                        ),
                        Cp = switch (life_stage,
                                     Egg = c(Y=0.015,Tmax=38.92,rho=0.155,V=5.766),
                                     phyloLarvae = c(Y=3*0.003,Tmax=37.58,rho=0.17,V=5.51),
                                     stemLarvae = c(Y=1.5*0.003,Tmax=37.58,rho=0.17,V=5.51),
                                     Pupae = c(Y=0.02,Tmax=38.04,rho=0.17,V=5.65)
                        )
  )
  r <-  parameters["Y"]*(exp(parameters["rho"]*T)-exp(parameters["rho"]*parameters["Tmax"]-((parameters["Tmax"]-T)/parameters["V"])))
  r
}

######## TO BE REMOVED ###################
developmentTime <- function(developmentRate) ######## TO BE REMOVED ###################
{
  DevTimeArray <- array(0,dim=dim(developmentRate),dimnames=dimnames(developmentRate))
  for (Day in 1:ncol(developmentRate))
  {
    day=0
    DevRateSum=developmentRate[,Day]/2;day=0
    DevTime=rep(0,nrow(developmentRate))
    while (any(DevRateSum<1))
    {
      day<-day+1
      DevTime[(DevRateSum<1)] <- DevTime[(DevRateSum<1)]+1
      DevRateSum2 <- DevRateSum+(developmentRate[,Day+day])*(DevRateSum<1)
      DevTime[(DevRateSum2>1)&(DevRateSum<1)] <- DevTime[(DevRateSum2>1)&(DevRateSum<1)]-(DevRateSum2-1)/developmentRate[,Day+day]
    }
    DevTimeArray[,Day] <- DevTime
  }
DevTimeArray
}

######## GOOD ONE ###################
developmentTime <- function(developmentRate)
{
 if ((class(developmentRate)=="array"|class(developmentRate)== "matrix")&(length(dim(developmentRate)==2))){
    cumRate <- developmentRate
    devTime <- array(NA,dim=dim(developmentRate),dimnames=dimnames(developmentRate))
    for (Day in 1:ncol(developmentRate))
    {
      j=1
      while (any(na.omit(cumRate[,Day]<1))&(Day-j>0))
      {
        NotDevelopped <- (cumRate[,Day]<1)
        cumRate[,Day] <- cumRate[,Day] + developmentRate[,Day-j]
        NewlyDevelopped <- (cumRate[NotDevelopped,Day]>=1)
        devTime[NewlyDevelopped,Day] <- j
        j=j+1
      }
    }
  } else {
    if (class(developmentRate)=="EnvTimeSeries") developmentRate <- values(getValues(developmentRate))
    if (class(developmentRate)=="RasterBrick") developmentRate <-values(developmentRate)
    if (class(developmentRate)=="RasterStack") developmentRate <-values(developmentRate)
      cumRate <- developmentRate
      devTime <- array(NA,dim=dim(developmentRate),dimnames=dimnames(developmentRate))
      for (Day in 1:ncol(developmentRate))
      {
        j=1
        while (any(na.omit(cumRate[,Day]<1))&(Day-j>0))
        {
          NotDevelopped <- which(cumRate[,Day]<1)
          cumRate[,Day] <- cumRate[,Day] + developmentRate[,Day-j]
          NewlyDevelopped <- (cumRate[NotDevelopped,Day]>=1)
          devTime[NewlyDevelopped,Day] <- j
          j=j+1
        }
      }
    }
 devTime 
  }

plantQuality <- function(env_time_series,variable,number_of_days)
{
  # Adds a plant quality layer onto Environemental Time serie
  #
  #
  env_time_serie <- getEnvTimeSerie(env_time_series,variable)
  env_time_array <- as.array(getValues(env_time_serie))
  env_time_array_nb_day_added <- env_time_array[,,c(rep(1,number_of_days-1),1:dim(env_time_array)[3])]
  plant_quality_array <- as.array(getValues(env_time_serie))
  for (day in 1:dim(env_time_array)[3]){
    plant_quality_array[,,day] <- rowMeans(env_time_array_nb_day_added[,,day:(day+number_of_days-1)],dims=2)
  }
  EnvTimeSerie(list(plant_quality_array,getDates(env_time_serie),"plantQ",extent(getValues(env_time_serie))))
}

timeSpendInAgeClass <-function(devRate) 
{
  # arguments:
  # - devRate : vector of development rate for each age class
  # Value : 
  # - vector of time spent in the age class = 1 /deVRate
  1/devRate
}

timeSpentInDayWhenLeavingAgeClass <- function(devRate)
{
  # arguments:
  # - devRate : vector of development rate for each age class
  # Value : 
  # - matrix of time spent in the day when leaving the age class 
  # row number gives the age class at 0h
  # column number gives the age class to be considered
  #
  # devRate=c(rep(2,10),rep(1.5,10),rep(.5,10),rep(5,10),rep(3,10))
  TimeSpentInEachClass=1/devRate
  b <- t(matrix(c(1:n,rep(0,n+1)),nrow=2*n+1,ncol=n,byrow=TRUE)[(1:n)*2-1,]) # 2 dimensions
  b <- matrix(1:n,nrow=n,byrow=TRUE)
  c <- array(array(c(b,rep(0,prod(dim(b))+dim(b)[1])),dim=c(2*n+n^2,n,n)),dim=c(n,n,2*n+2))[,,2*(1:n+1)] # 3 dimensions
  t(rowSums(c,dims=2))
  
  TimeSpentInEachClass=1/devRate;n=length(TimeSpentInEachClass)
  b <- t(matrix(c(TimeSpentInEachClass,rep(0,n+1)),nrow=2*n+1,ncol=n,byrow=TRUE)[(1:n)*2-1,]) # 2 dimensions
  b <- matrix(1:n,nrow=n,ncol=n,byrow=TRUE)*upper.tri(matrix(1,nrow=n,ncol=n))
  c <- array(array(c(b,rep(0,prod(dim(b))+dim(b)[1])),dim=c(2*n+n^2,n,n)),dim=c(n,n,2*n+2))[,,2*(1:n+1)] # 3 dimensions
  TR <- t(rowSums(c,dims=2))
  TR<1

}


numericalResponse <- function(ageStructure,EnvDay)
{
  # arguments :
  # EcolData
  # devRates = matrix of development rate per unit of time [deme, stage]
  # egg_layed = matrix of number of eggs layed per adult 
  # egg_survival = matrix or scalar of egg survival probability per day
  # value : transition matrix 
  #
  ageStructure
}

