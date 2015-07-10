setClass("EcoDay",
         representation(values = "array",dates = "Date", envDay = "RasterBrick", stages="character"),
         prototype(values = plot(hist(round(rgamma(20,5,2))),main="ecol data age classes",breaks=11,xlab="age")),
         validity = function(object){
           if (length(object@dates)!=1) stop("EcoDay has to contain one and only one date") else {
             if (!all(dim(object@values)[1:2]==dim(object@envDay)[1:2])) stop("lanscape dimensions of ecolarray and envDay raster brick arguments do not match") else {
              if (dim(object@values)[3]!=length(object@stages)) stop ("array and age classes dimensions do not fit") else {}
             }
           }
           return(TRUE)
         }
)

setMethod(f="getArray",
          signature = "EcoDay",
          definition = function(object1,object2){
            if (is.null(object2)) return(object1@values) else {
              object1@values[,,object2]
            }
          })

setMethod(f="getMatrix",
          signature = "EcoDay",
          definition = function(object1,object2){
            if ((class(object1)!= "EcoDay")|(class(object2)!= "character"))stop("object1 is not an 'EcoDay' or object2 is not a 'character' ! ")
            matrix(values(raster(getArray(object1,which(getStage(object1)==object2)[1])))%*%m,nrow=landDim[1],ncol=landDim[2],byrow=TRUE)
          })


setMethod(f="getEnvDay",
          signature = "EcoDay",
          definition = function(object){
            return(object@envDay)
          })

setMethod(f="getStage",
          signature = "EcoDay",
          definition = function(object){
            return(object@stages)
          })

setMethod(f="mySetValues",
          signature = "EcoDay",
          definition = function(object,newValues,Subset){
            # object : the EcoDay variable to set
            # newValues : the new values
            # Subset : the part of ecoday to set by new values (age class vector)
            if (((class(newValues)=="numeric")|(class(newValues)=="array"))&(class(Subset)=="character")) {
              if (Subset=="all") object@values = array(newValues,dim(object@values))
              if (Subset%in%object@stages) { object@values[,,which(object@stages%in%Subset)] = 
                                               array(newValues,c(dim(object@values)[1:2],length(which(object@stages%in%Subset))))
              }
            }
            if (class(Subset)=="numeric") Subset <- as.integer(Subset)
            if ((class(newValues)=="array")&(length(Subset)==1)) newValues<-matrix(newValues,nrow=dim(newValues)[1],ncol=dim(newValues)[2])
            if ((class(newValues)=="matrix")&(length(Subset)==1)&(class(Subset)=="integer")) {
              object@values[,,Subset] = newValues
            }
            if ((class(newValues)=="array")&(class(Subset)=="integer")&(length(Subset)!=1)) { 
              if (any(dim(newValues)!=c(dim(object@values)[1:2],length(Subset)))) stop("new values array has wrong dimension")
              object@values[,,Subset] = newValues
            }
            return(object)
          })

setMethod(f="myAddValues",
          signature = "EcoDay",
          definition = function(object,newValues,Subset){
            # object : the EcoDay variable to set
            # newValues : the new values
            # Subset : the part of ecoday to set by new values (age class vector)
            if (((class(newValues)=="numeric")|(class(newValues)=="array"))&(class(Subset)=="character")) {
              if (Subset=="all") object@values = object@values+array(newValues,dim(object@values))
              if (Subset%in%object@stages) { object@values[,,which(object@stages%in%Subset)] = object@values[,,which(object@stages%in%Subset)] +
                                               array(newValues,c(dim(object@values)[1:2],length(which(object@stages%in%Subset))))
              }
            }
            if (class(Subset)=="numeric") Subset <- as.integer(Subset)
            if ((class(newValues)=="array")&(length(Subset)==1)) newValues<-matrix(newValues,nrow=dim(newValues)[1],ncol=dim(newValues)[2])
            if ((class(newValues)=="matrix")&(length(Subset)==1)&(class(Subset)=="integer")) {
              object@values[,,Subset] = object@values[,,Subset]+newValues
            }
            if ((class(newValues)=="array")&(class(Subset)=="integer")&(length(Subset)!=1)) { 
              if (any(dim(newValues)!=c(dim(object@values)[1:2],length(Subset)))) stop("new values array has wrong dimension")
              object@values[,,Subset] = object@values[,,Subset]+newValues
            }
            return(object)
          })

EcoDay <- function(x) 
{
  # Arguments: a list
  # - [[1]] an array [X,Y,Age_Stage_Class]
  # - [[2]] a one day Date class variable 
  # - [[3]] an raster brick class (X,Y dimension should correspond with the array) with environmental values of the day 
  # - [[4]] the vector stage name for each age class (length = the number of age classes)
new("EcoDay",values=x[[1]],dates=x[[2]],envDay=x[[3]],stages=x[[4]])
}
