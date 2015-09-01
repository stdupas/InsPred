setClass("EnvTimeSeries",
         representation(values = "list"),
         prototype(values = plot(brick(raster(matrix(1)),raster(matrix(0)),as.Date(0,1)))),
#         contains = "EnvTimeSerie",
         validity = function(object){
           if (any(lapply(object@values,class)!="EnvTimeSerie")){
             stop("list in arguments of new values is not a list of EnvTimeSerie")
             #           } else if (any(lapply(object@values,getDates)!=getDates(object@values[[1]]))) {
             #             stop("dates do not match in new EnvTimeSeries construction")
             } else if (!prod(as.data.frame(lapply(object@values,etsDim))==as.data.frame(lapply(object@values,etsDim))[,1])) {
            stop("dates do not match in new EnvTimeSeries construction")
           }
           return(TRUE)
         }
)

setMethod(f="getDates",
          signature = "EnvTimeSeries",
          definition = function(object){
            return(getValues(object)[[1]]@dates)
          })

setMethod(f="getValues",
          signature = "EnvTimeSeries",
          definition = function(object){
            return(object@values)
          })

setMethod("etsDim",
          signature = "EnvTimeSeries",
          function(object){
            return(lapply(object@values,etsDim))
          })

setMethod("getVarNames",
          signature = "EnvTimeSeries",
          function(object){
            return(unlist(lapply(object@values,getVarNames)))
          })

setMethod(f="getDays",
          signature = "EnvTimeSeries",
          definition = function(object1,object2){
            if (class(object2)=="character") object2 <- as.Date(object2)
            if (class(object2)=="Date") object2 <- which(getDates(object1)==object2)
            if (class(object2)=="numeric") object2 <- as.integer(object2)
            if (class(object2)!="integer") stop ("wrong class for second argument")
            tmp <-brick(lapply(lapply(getValues(object1),getValues),function(x) subset(x,object2)))
            names(tmp) <- getVarNames(object1)
            return(tmp)
          })

setMethod("getEnv",
          signature = "EnvTimeSeries",
          function(object1,object2){
            return(getValues(object1)[[which(getVarNames(object1)==object2)]])
          })

EnvTimeSeries <- function(x)
{ names(x) <- unlist(lapply(x,getVarNames))
  # Arguments: a list of EntTimeSerie class variable
  new("EnvTimeSeries",values=x)
}
