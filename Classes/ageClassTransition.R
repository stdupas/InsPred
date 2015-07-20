setClass("ageClassTransition",
         representation(transition = "array", stages="character", dates="Date", extent="Extent", res="numeric"),
         prototype(transition = plot(raster(matrix(c(0.1,0.9,0,0,0,0,0,0.2,0.8,0,0,0,0,0,.5,.5,0,0,0,0,0,0.8,0.2,0,0,0,0,0,.9,.1,0,0,0,0,0,.1),nrow=6,ncol=6,byrow=TRUE)))),
         validity = function(object){
           # transtion = array dim(number of X, number of Y, number of dates, number of age classes, number of age classes)
           if (dim(transtion)[3]!=length(dates)) stop("third dimension of transition array is not equal to number of dates")
           if ((dim(transtion)[4]!=length(stages))|(dim(transtion)[5]!=length(stages))) stop("fourth and/or fifth dimension of transiton array does not have the same length as stages vector")
         }
)

setMethod(f="getDates",
          signature = "ageClassTransition",
          definition = function(object){
            return(object@dates)
          })

ageClassTransition <- function(envtimeserie, developmentRateFunction, stages, dates, Species)
{
  # Constructor of age class transition matrix
  # slots : 
  # transition = array of transition : dimensions X, Y, Date, Classe d'age, Classe d'age du jour suivant, probabilité associée
  # stages
  # dates
  # extent
  # resolution
#   developmentRateLogan(getValues(Tmean),"Bf","Egg")
#   envtimeserie = Tmean; developmentRateFunction = developmentRateLogan; stages = stages; dates = dates; Species = "Bf"
  
  ### Formatting dates
  if (class(dates)=="character") {dates = as.Date(dates)}
  if (class(dates)=="Date"){
    dates=which(getDates(Tmean)%in%dates)
  }
  
  ### Initialize 
  ageclasstransition <- array(0, dim = c(dim(getValues(envtimeserie)),length(stages),2))
  # to put in a method of EcoDay
  vec <- 1:length(levels(as.factor(stages)))
  number_of_age_class_per_stage <- vec
  
  for (Stage in vec) {
    number_of_age_class_per_stage[Stage] <- sum( stages == levels(as.factor(stages))[Stage]) 
  }
  names(number_of_age_class_per_stage) <- levels(as.factor(stages))
  
  ### Fill the array for transition between substages of eggs/larvae/adult
  transition <- array(0,dim=c(dim(getValues(Tmean))[1],dim(getValues(Tmean))[2],length(dates),length(stages)-1,2))
  cumT <- 0
  for (X in 1:dim(getValues(Tmean))[1]){ # X <- 1 
    for (Y in 1:dim(getValues(Tmean))[2]){ # Y <- 1
      for (i_date in dates){ # i_date <- dates[1]
        for (C1 in 1:(length(stages)-1)){ # C1 <- 1
          C2 <- C1
          while ((cumT<1)&(C2<length(stages)))
            {
            C2 <- C2 +1
            devRate_v <-  vapply(X = getValues(Tmean)[X,Y][1, i_date],
                                 FUN = developmentRateFunction,
                                 species = Species,
                                 life_stage = stages[C2],
                                 FUN.VALUE = c(1))
            number_of_age_class_per_stage[stages[C2]]/devRate_v
            T <- 1/(devRate_v*number_of_age_class_per_stage[stages[C2]])
            cumT = cumT + T
            C2 <- C2+1
            }
          transition[X,Y,i_date,C1,1] <- C2-1
          transition[X,Y,i_date,C1,2] < 1-1/(cumT-T)
          }
        }
      }
    }
new("ageClassTransition", list(transition=transition,stages,dates,extent(getValues(envtimeserie)),res))
}