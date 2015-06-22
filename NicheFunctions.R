
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
                                     Egg = c(alpha=117.33, k=3474.17, b=0.13, Tmin=4.54, Tmax=56.78, dt=6.22),
                                     Larvae = c(Y=0.001,Tmax=31.229,rho=0.116,V=1.654),
                                     Pupae = c(Y=0.006,Tmax=33.039,rho=0.164,V=5.227)
                        ),
                        Sc = switch (life_stage,
                                     Egg = c(Y=0.010,Tmax=31.93,rho=0.110,V=1.786),
                                     Larvae = c(Y=0.002,Tmax=35.79,rho=0.150,V=5.695),
                                     Pupae = c(Y=0.009,Tmax=35.09,rho=0.170,V=5.427)
                        ),
                        Cp = switch (life_stage,
                                     Egg = c(Y=0.015,Tmax=38.92,rho=0.155,V=5.766),
                                     Larvae = c(Y=0.003,Tmax=37.58,rho=0.17,V=5.51),
                                     Pupae = c(Y=0.02,Tmax=38.04,rho=0.17,V=5.65)
                        )
  )
  if ((species == "Bf") & (life_stage == "Egg")) {r <- parameters["alpha"]*((1/(1+parameters["k"]*exp(-parameters["b"]*(T-parameters["Tmin"]))))-exp((parameters["Tmax"]-(T-parameters["Tmin"]))/parameters["dt"]))} else {
    r <-  parameters["Y"]*(exp(parameters["rho"]*T)-exp(parameters["rho"]*parameters["Tmax"]-((parameters["Tmax"]-T)/parameters["V"])))
  }
  r
}

