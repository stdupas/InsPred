# varName : either stage for non environmental model or environmental variale if environemental model
# fun = function to combine submodels
# stages = stages concerned by the model
# submodel = list of submodels (class model or parameter values)
# supermodel = whether the model is a list of model (TRUE) or a model (FALSE)
# environemental = whether the model depends on environmental values
setClass("model", representation(varName = "character",
                                 fun = "function",
                                 stages = "character",
                                 submodel = "list",
                                 supermodel = "logical",
                                 environmental = "logical"),
         prototype(varName = "Undefined",
                   fun = function(x) x,
                   stages="Egg",
                   submodel = list(1,2)
                   ),
         validity = function(object) { ## object : nom reserve !
           if (!(all(lapply(object@submodel,class)=="numeric")|all(lapply(object@submodel,class)=="model"))) stop("slot submodel is not of class model or of class numeric")
           if (all(lapply(object@submodel,class)=="numeric")&(object@supermodel!=FALSE)) stop("slot supermodel is TRUE but slot submodel is not a list of models")
           if (all(lapply(object@submodel,class)=="model")&(object@supermodel!=TRUE)) stop("slot supermodel is FALSE but slot submodel is not a list of parameters")
           #      if (length(as.list(object@fun))!=3) stop("function in the model does not have two arguments in its definition")
         }
)


#setMethod(
  #f="ToStream",
  #signature = "model",
  #definition = function(object){
   # ToStream(object)
  #}
#)

setMethod(
  f = "ToStream",
  signature = "model",
  definition = function(object){
    cat ("varName = " , object@varName, "\n")
    cat("fun = ") 
    show(object@fun) 
    cat("stages = ",object@stages,"\n") 
    cat("submodel = \n")
    show(object@submodel)
    cat("supermodel = ", object@supermodel )
  }
)


setMethod(
  f = "applyModel",
  signature = "model",
  definition =  function(object,ecoDay){
    if (length(as.list(object@fun))==1&object@supermodel==FALSE) return (do.call(what = object@fun,
                                                                                 args =list()))
    if (object@supermodel==FALSE) {
      if (object@environmental==TRUE) {
        result <- myOperation(ecoDay,
                              getEnv(ecoDay,object@varName)*unlist(object@submodel),
                              object@stages,
                              object@fun)
        #result <- do.call(what = object@fun,
        #                  args = list(getEnv(ecoDay,object@varName),unlist(object@submodel)))
        } else {
          result <- myOperation(ecoDay,object@submodel,object@stages,object@Fun)    }
              #result <- do.call(what = object@fun,
              #                  args = list(getArray(ecoDay,object@stages),unlist(object@submodel)))}
      } else {
        result <- do.call("myOperation",append(lapply(object@submodel,applyModel,ecoDay),list(object@stages,object@fun)))
                                                       # apply object@fun to the list of submodels values
    }
    #result <- myOperation(ecoDay,result,object@stages,"*")
    return(result)
  })

setMethod(f="getVarNames",
          signature = "model",
          definition = function(object){
            return(object@varName)
          })

model <- function(varName=NA, Fun, stages, submodel, supermodel=NA, environmental=TRUE){
  if (is.na(supermodel)) supermodel = all(lapply(submodel,class)=="model")
  if (is.na(varName)&supermodel==FALSE&length(as.list(Fun))!=1) stop ("varName is missing")
  if (is.na(varName)&supermodel==FALSE&length(as.list(Fun))==1) varName = "no argument"
  if (is.na(varName)&supermodel==TRUE) varName=as.character(as.factor(unlist(lapply(submodel,getVarNames))))
  #
  new("model", varName = varName, fun = Fun, stages = stages, submodel = submodel,supermodel = supermodel, environmental=environmental)
}
