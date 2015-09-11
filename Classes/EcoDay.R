setClass("EcoDay",
         representation(values = "array",dates = "Date", envDay = "RasterBrick", stages="character",variables="character"),
         prototype(values = plot(hist(round(rgamma(20,5,2))),main="ecol data age classes",xlab="age")),
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

setMethod(f="getValues",
          signature = "EcoDay",
          definition = function(object,ACSubset=NULL){ # object2 is an age class
            if(class(ACSubset)=="character") ACSubset = which(getStage(object)%in%ACSubset)
            if (is.null(ACSubset)) return(object@values) else {
              object@values[,,ACSubset]
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
            matrix(values(raster(getValues(object1,which(getStage(object1)==object2)[1])))%*%object3,nrow=landDim[1],ncol=landDim[2],byrow=TRUE)
          })

setMethod(f="getDates",
          signature = "EcoDay",
          definition = function(object){
            return(object@dates)
          })


setMethod(f="getEnv",
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
          definition = function(object,newValues,ACSubset){
            # object : the EcoDay variable to set
            # newValues : the new values
            # ACSubset : the age class vector corresponding to the part of ecoday to be set 
            if (((class(newValues)=="numeric")|(class(newValues)=="array"))&(class(ACSubset)=="character")) {
              if (ACSubset=="all") object@values = array(newValues,dim(object@values))
              if (ACSubset%in%object@stages) { object@values[,,which(object@stages%in%ACSubset)] = 
                                               array(newValues,c(dim(object@values)[1:2],length(which(object@stages%in%ACSubset))))
              }
            }
            if (class(ACSubset)=="numeric") ACSubset <- as.integer(ACSubset)
            if ((class(newValues)=="array")&(length(ACSubset)==1)) newValues<-matrix(newValues,nrow=dim(newValues)[1],ncol=dim(newValues)[2])
            if ((class(newValues)=="matrix")&(length(ACSubset)==1)&(class(ACSubset)=="integer")) {
              object@values[,,ACSubset] = newValues
            }
            if ((class(newValues)=="array")&(class(ACSubset)=="integer")&(length(ACSubset)!=1)) { 
              if (any(dim(newValues)!=c(dim(object@values)[1:2],length(ACSubset)))) stop("new values array has wrong dimension")
              object@values[,,ACSubset] = newValues
            }
            return(object)
          })


setMethod(f="myOperation",
          signature = "EcoDay",
          definition = function(object1,object2,Fun,ACSubset=NULL){
            # object1 : the EcoDay variable to set
            # object2 : the other object to do the myOperation with (can be numeric, raster, array, Ecoday, ageClassTransition)
            # Fun : the operator used for combining the two objects ('+','*','-','/'...)
            # ACSubset : the part of ecoday to set by object2 (age class vector)
            # 
            if (is.null(ACSubset)) ACSubset = getStage(object1) else {
              if(class(ACSubset)=="numeric") ACSubset = as.integer(ACSubset)
              if(class(ACSubset)=="character") ACSubset = which(getStage(object1)%in%ACSubset)
            }
            if ((class(object2)=="RasterLayer")|(class(object2)=="RasterStack")|(class(object2)=="RasterBrick")) {
              object2 <- array(as.array(object2),c(dim(object1@values)[1:2],length(ACSubset)))
            }
            if ((class(object2)=="array")|(class(object2)=="matrix")|(class(object2)=="numeric")){
              object1@values[,,ACSubset] = do.call(Fun,list(object1@values[,,ACSubset],object2))
            }
            if (class(object2)=="EcoDay") {
              object1@values[,,ACSubset] = 
                do.call(Fun,
                        list(object1@values[,,ACSubset],
                             object2@values[,,ACSubset])
                )
            }
            if (class(object2)=="ageClassTransition") {
              if (!(getDates(object1)%in%getDates(object2))) stop("date of Ecoday argument is not in ageClassTransition argument")
              nb_stages <- length(getStage(object))
              ClassBefore = c(nb_stages,1:(nb_stages-1))
              currentTransition <- getValue(object2)[,,getDates(EcoDay),,]
              object1@values[,,currentTransition[,,,"NextDayUpperStage"]] <-
                currentTransition[,,,"AssociatedProbability"] * getDates(object1)
              object1@values[,,ClassBefore(currentTransition[,,,"NextDayUpperStage"])] <-
                object1@values[,,] + (1-currentTransition[,,,"AssociatedProbability"]) * getDates(object1)
            }
            return(object1)
          })


setMethod(f="recruitment",
          signature = "EcoDay",
          definition = function(object,fecun){
            #object@values
            object <- myOperation(object,rowSums(object@values[,,which(object@stages=="Adult")]*fecun,dims=2),'+',ACSubset=1)
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
