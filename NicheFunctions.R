
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
                                     Egg = c(logan4)
                                     Larvae = c(Y=0.001,Tmax=31.229,rho=0.116,V=1.654),
                                     Pupae = c(Y=0.006,Tmax=33.039,rho=0.164,V=1.654)
                        ),
                        Sc = switch (life_stage,
                                     Egg = c(Y=0.001,Tmax=31.229,rho=0.116,V=1.654),
                                     Larvae = c(Y=0.001,Tmax=31.229,rho=0.116,V=1.654),
                                     Pupae = c(Y=0.006,Tmax=33.039,rho=0.164,V=1.654)
                        ),
                        Cp = switch (life_stage,
                                     Egg = c(Y=0.001,Tmax=31.229,rho=0.116,V=1.654),
                                     Larvae = c(Y=0.001,Tmax=31.229,rho=0.116,V=1.654),
                                     Pupae = c(Y=0.006,Tmax=33.039,rho=0.164,V=1.654)
                        ),
  )
  if (species == "Bf")&(life_stage=="Egg") {r <- } else {
    r <-  parameters["Y"]*(exp(parameters["Rho"]*T)-exp(paramters["Rho"]*parameters["Tmax"]))
  }
  r
}