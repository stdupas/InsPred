
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
