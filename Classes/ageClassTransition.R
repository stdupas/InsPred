setClass("ageClassTransition",
         slots=c(transition = "array", stages="character", dates="numeric"),
         #prototype(transition = plot(raster(matrix(c(0.1,0.9,0,0,0,0,0,0.2,0.8,0,0,0,0,0,.5,.5,0,0,0,0,0,0.8,0.2,0,0,0,0,0,.9,.1,0,0,0,0,0,.1),nrow=6,ncol=6,byrow=TRUE)))),
         validity = function(object){
           # transtion = array dim(number of X, number of Y, number of dates, number of age classes, number of age classes)
           if (dim(transition)[3]!=length(dates)) stop("third dimension of values array is not equal to number of dates")
           #if ((dim(transition)[4]!=length(stages))|(dim(transition)[5]!=length(stages))) stop("fourth and/or fifth dimension of transiton array does not have the same length as stages vector")
         }
)

setMethod(f="getDates",
          signature = "ageClassTransition",
          definition = function(object){
            return(object@dates)
          })

ageClassTransition <- function(x)
  # x=list(envtimeserie, developmentRateFunction, stages, dates, Species,transition)
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
  if (class(x[[4]])=="character") {x[[4]] = as.Date(x[[4]])}
  if (class(x[[4]])=="Date"){
    x[[4]]=which(getDates(x[[1]])%in%x[[4]])
  }
  
  ### Initialize 
  ageclasstransition <- array(0, dim = c(dim(getValues(x[[1]])),length(x[[3]]),2))
  # to put in a method of EcoDay
 
  vec <- 1:length(levels(as.factor(x[[3]])))
  number_of_age_class_per_stage <- vec
  
  for (Stage in vec) {
    number_of_age_class_per_stage[Stage] <- sum( x[[3]] == levels(as.factor(x[[3]]))[Stage]) 
  }
  names(number_of_age_class_per_stage) <- levels(as.factor(x[[3]]))
  
  ### Fill the array for transition between substages of eggs/larvae/adult
  transition <- array(0,dim=c(dim(getValues(x[[1]]))[1],dim(getValues(x[[1]]))[2],length(x[[4]]),length(x[[3]])-1,2))
  cumT <- 0
  for (X in 1:dim(getValues(x[[1]]))[1]){ # X <- 1 
    for (Y in 1:dim(getValues(x[[1]]))[2]){ # Y <- 1
      for (i_date in x[[4]]){ # i_date <- x[[4]][1]
        for (C1 in 1:(length(x[[3]])-1)){ # C1 <- 1
          C2 <- C1
          while ((cumT<1)&(C2<length(x[[3]])))
            {
            C2 <- C2 +1
            devRate_v <-  vapply(X = getValues(x[[1]])[X,Y][1, i_date],
                                 FUN = x[[2]],
                                 species = x[[5]],
                                 life_stage = x[[3]][C2],
                                 FUN.VALUE = c(1))
            number_of_age_class_per_stage[x[[3]][C2]]/devRate_v
            T <- 1/(devRate_v*number_of_age_class_per_stage[x[[3]][C2]])
            cumT = cumT + T
            C2 <- C2+1
            }
          transition[X,Y,i_date,C1,1] <- C2-1
          transition[X,Y,i_date,C1,2] <- 1-1/(cumT-T)
         
        }
        }
      }
    }
x[[6]] <- transition
new("ageClassTransition",x[[6]],x[[3]],x[[4]]) #,extent(getValues(envtimeserie)),res
}
