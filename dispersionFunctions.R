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

distanceMatrixFromRaster2 =
  function(object){
    # Computes a pairwise distance matrix from a raster object
    #
    # Args:
    #   object: a raster object from which computes distances
    #
    # Returns:
    #   A matrix of distances in meters if a coordinate system is precised
    
    # Extract coordinates from raster object
    coords = xyFromCell(object = object, cell = 1:ncell(object), spatial=FALSE)
    lat = coords[,1]
    lon = coords[,2]
    # Compute distance matrix
    dist = NULL
    for(i in 1:length(lat)) {
      res = NULL
      for(j in 1:length(lon)) {
        res = c(res,sphericDistance(lat[i],lat[j],lon[i],lon[j]))
      }
      dist = rbind(dist,res)
    }
    return(dist)
  }

sphericDistance <- function (lat1, lat2, lon1, lon2) 
{
  if (lat1 == lat2 & lon1 == lon2) 
    distance <- 0
  else {
    rlat1 = radian(lat1)
    rlat2 = radian(lat2)
    rlon = radian(lon2 - lon1)
    distance <- 60 * (180/pi) * acos(sin(rlat1) * sin(rlat2) + 
                                       cos(rlat1) * cos(rlat2) * cos(rlon))
    distance <- distance * 1852/1000
  }
  distance
}

radian <- function (degree) 
{
  radian <- degree * (pi/180)
  radian
}