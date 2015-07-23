setClass("SuperModel",
         representation(operator="character",stages="character",models="list"),
         prototype(operator="+",stages=c("Egg"),models = list(model(varName="Tmin", fun=conquadraticSkewed1), model(varName="Tmax", fun=conquadraticSkewed1))),
         validity = function(object) { ## object : nom reserve !
           if (class(try(do.call(operator,list(1,2))))=="try-error") stop("super model operator is not an operator")
           if (any(lapply(models,FUN="class")!="model")) stop("some models are not of model class")
         }
)

setMethod(
  f = "show",
  signature = "SuperModel",
  definition = function(object){
    ToStream(object)
  }
)

setMethod(
  f="ToStream",
  signature = "SuperModel",
  definition = function(object){
    for(i in 1:length(object@models)){
      ToStream(object@models[[i]])
    }}
)

setMethod(
  f = "applyModel", 
  signature = "SuperModel",
  definition = function(object){
    rawList <- lapply(X = object@models,
                      FUN = applyModel)
    meanVal <- Reduce("+", rawList) / length(rawList)
    return(meanVal)
  })












