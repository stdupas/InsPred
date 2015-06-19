fatTail1 <- function(x, alpha, beta){
  # Computes a value for kernel dispersion using a fat tail model, according to the method in ?
  #
  # Args: 
  #   x: the distance between two points
  #   alpha : the first parameter of the dispersion law
  #   beta: the second parameter of the dispersion law
  #
  # Returns:
  #   The value of dispersion kernel for x
  return(1/(1+1/alpha *x^beta))
}

migrationRateMatrix <- function(dispersion){
  # Normalizes a matrix of dispersion kernel between cells to get a migration rate matrix between cells.
  #
  # Args:
  #   dispersion: a matrix representing the values of a specified kernel (function of distances between cells)
  #
  # Returns:
  #   A migration rate matrix (note that rowSums and colSums are not 1: cause of bordure effect, individuals go "out of the world")
  return(dispersion/max(c(colSums(dispersion),rowSums(dispersion))))
}
