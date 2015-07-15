setClass("ageClassTransition",
         representation(transition = "array", stages="character", dates="Date", extent="Extent", res="numeric"),
         prototype(transition = plot(raster(matrix(c(0.1,0.9,0,0,0,0,0,0.2,0.8,0,0,0,0,0,.5,.5,0,0,0,0,0,0.8,0.2,0,0,0,0,0,.9,.1,0,0,0,0,0,.1),nrow=6,ncol=6,byrow=TRUE)))),
         validity = function(object){
           # transtion = array dim(number of X, number of Y, number of dates, number of age classes, number of age classes)
           if (dim(transtion)[3]!=length(date)) stop("third dimension of transition array is not equal to number of dates")
           if ((dim(transtion)[4]!=length(stages))|(dim(transtion)[5]!=length(stages))) stop("fourth and/or fifth dimension of transiton array does not have the same length as stages vector")
         }
)

setMethod(f="getDates",
          signature = "ageClassTransition",
          definition = function(object){
            return(object@dates)
          })

ageClassTransition <- function(envtimeserie,developmentRateFunction,stages,Dates,Species)
{
  # Constructor of age class transition matrix
  # slots : 
  # transition = array of transition : dimensions X, Y, Date, Classe d'age, Classe d'age du jour suivant, probabilité associée
  # stages
  # dates
  # extent
  # resolution
  
  if (class(Dates)=="character") {Dates=as.Date(Dates)}
  if (class(Dates)=="Dates"){
    Dates=which(getDates(Tmean)%in%Dates)
  }
  #  StClTr <- array(0,dim=c(dim(getValues(Tmean)),length(stages),length(stages)))
  ageclasstransition <- array(0, dim = c(dim(getValues(envtimeserie)),legnth(stages),2))
  
  ### to put in a method of EcoDay
  number_of_age_class_per_stage=NA
  for (Stage in 1:length(levels(as.factor(stages)))) { number_of_age_class_per_stage[Stage] <- sum( stages==levels(as.factor(stages))[Stage]) 
  names(number_of_age_class_per_stage)=levels(as.factor(stages))}
  
  
  ### to modify
  transition <- list()
  for (X in 1:dim(getValues(Tmean))[1]){
    for (Y in 1:dim(getValues(Tmean))[2]){
      for (Date in Dates){
        cumT <- 0
        for (C1 in 1:(length(stages)-1)){
          C2 <- C1
          while ((cumT<1)&(C2<length(stages)))
            {
            C2 <- C2 +1
            T <- 1/(sapply(getValues(Tmean)[X,Y][1,Date],developmentRateFunction,species=Species,life_stage=stages[C2])*number_of_age_class_per_stage[stages[C2]])
            cumT=cumT+T
            C2 <- C2+1
            }
          transition[X,Y,Date,C1,1] <-C2-1
          transition[X,Y,Date,C1,2] < 1-1/(cumT-T) 
          }
        }
      }
    }
          
  }
new("ageClassTransition", list(transtion=transition,stages ,Dates,extent(getValues(envtimeserie)),))
}