setClass("model", representation(varName = "character",
                                 fun = "function"),
         prototype(varName = "Undefined",
                   fun = function(x) x),
         validity = function(object) { ## object : nom reserve !
           if (length(object@varName)!=1) stop("model does not have one value")
           if (length(as.list(object@fun))!=3) stop("function in the model does not have two arguments")
         })

setMethod(
  f = "applyModel",
  signature = "model",
  definition =  function(object,varEnv,Parameters){
    Parameters <- list(x=varEnv,Parameters)
    return(do.call(what = object@fun, args = Parameters))
  })

model <- function(varName, fun){
  if(missing(varName)){
    varName <- deparse(substitute(varEnv))
  }
  new("model", varName = varName, fun = fun)
}
