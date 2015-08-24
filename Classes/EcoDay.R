setClass("EcoDay",
         representation(values = "array",dates = "Date", envDay = "RasterBrick", stages="character",variables="character"),
         prototype(values = plot(hist(round(rgamma(20,5,2))),main="ecol data age classes",breaks=11,xlab="age")),
         validity = function(object){
           ifelse(length(object@dates)!=1, 
                  stop("EcoDay has to contain one and only one date"),
                  ifelse(!all(dim(object@values)[1:2]==dim(object@envDay)[1:2]),
                         stop("lanscape dimensions of ecolarray and envDay raster brick arguments do not match"),
                         ifelse(dim(object@values)[3]!=length(object@stages),
                                stop ("array and age classes dimensions do not fit"),
                                ifelse (any(names(object@envDay)!=object@variables),
                                        stop ("envDay names do not correspond to variables"),
                                        return(TRUE)))))
         }
)

setMethod(f="getArray",
          signature = "EcoDay",
          definition = function(object1,Subset){ # object2 is an age class
            if(class(Subset)=="character") Subset = which(getStage(object1)%in%Subset)
            if (is.null(Subset)) return(object1@values) else {
              object1@values[,,Subset]
            }
          })

setMethod(f="getVarNames",
          signature = "EcoDay",
          definition = function(object){
            return(object@variables)
          })

setMethod(f="getMigratedMatrix",
          signature = "EcoDay",
          # Used for calculating 'Current' migration for 'Adult' stage.
          # This methode is used in Inference
          definition = function(object1,object2,object3){
            if ((class(object1)!= "EcoDay")|(class(object2)!= "character"))stop(" either object1 is not an 'EcoDay' or object2 is not a 'character' ! ")
            matrix(values(raster(getArray(object1,which(getStage(object1)==object2)[1])))%*%object3,nrow=landDim[1],ncol=landDim[2],byrow=TRUE)
          })


setMethod(f="getEnvDay",
          signature = "EcoDay",
          definition = function(object1,object2=NULL){
            ifelse (is.null(object2), return(object1@envDay), return(object1@envDay[[object2]]))
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

setMethod(f="myOperation",
          signature = "EcoDay",
          definition = function(object1,object2,Subset,Fun){
            # object1 : the EcoDay variable to set
            # object2 : the other object to do the myOperation with
            # Subset : the part of ecoday to set by object2 (age class vector)
            # Fun : the operator used for combining the two objects ('+','*','-','/'...)
            # 
            if(class(Subset)=="numeric") Subset = as.integer(Subset)
            if(class(Subset)=="character") Subset = which(getStage(object1)%in%Subset)
            if ((class(object2)=="RasterLayer")|(class(object2)=="RasterStack")|(class(object2)=="RasterBrick")) {
              object2 <- array(as.array(object2),c(dim(object1@values)[1:2],length(Subset)))
            }
            if ((class(object2)=="array")|(class(object2)=="matrix")|(class(object2)=="numeric")){
              object1@values[,,Subset] = do.call(Fun,list(object1@values[,,Subset],object2))
            }
            if (class(object2)=="EcoDay") {
              object1@values[,,Subset] = 
                do.call(Fun,
                        list(object1@values[,,Subset],
                             object2@values[,,Subset])
                )
            }
            return(object1)
          })
          

setMethod(f="recruitment",
          signature = "EcoDay",
          definition = function(object,fecun){
            #object@values
            object <- myOperation(object,rowSums(object@values[,,which(object@stages=="Adult")]*fecun,dims=2),Subset=1,'+')
            return(object)                         
          }) # between age stages (adtults to egg first age class)

setMethod(
  f = "ToStream",
  signature = "EcoDay",
  definition = function(object){
    cat("slot 'array' dim = ", dim(object@values),
        
        "\nslot 'dates' ", object@dates,
        "\nslot 'envDay' ")
    show(object@envDay)
        cat("slot 'variables' ",unlist(object@variables))
  }
)



setMethod(f="survival",
          signature = "EcoDay",
          definition = function(object,density_dependence){
            if (density_dependence == FALSE){
              
            }
            return(object)
          }) # within age class independent of density

EcoDay <- function(x) 
{
  # Arguments: a list
  # - [[1]] an array [X,Y,Age_Stage_Class]
  # - [[2]] a one day Date class variable 
  # - [[3]] a raster brick class (X,Y dimension should correspond with the array) with environmental values of the day 
  # - [[4]] the vector stage name for each age class (length = the number of age classes)
new("EcoDay",values=x[[1]],dates=x[[2]],envDay=x[[3]],stages=x[[4]],variables=x[[5]])
}
