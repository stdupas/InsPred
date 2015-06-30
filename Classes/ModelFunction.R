setClass("ModelFunction",
         representation(name = "character",
                        fun = "function",
                        param = "list"),
         prototype(name = "", fun = function(x){}, param = list()),
         validity = function(object) { ## object : nom reserve !
           if (! all(names(formals(object@fun))[-1] == names(object@param))){
             return("\n Improper function parameters. 
                    \n The list names you entered for \"param\" argument should match with \"fun\" arguments.
                    \n Please check parameter names using \" formals() \" on the function passed to \"fun\" ")
           }else{
             return(TRUE)
           }
         }
)

ModelFunction <- function(name, fun, param){
  
  if(missing(name)) name <- deparse(substitute(fun))
  if(missing(fun)) stop("argument fun is missing")
  
  # if param is a list of numeric, it's ok
  # if param is a list of ModelFunction, apply the function as a prior
  areAllParamNum <- all(vapply(X = param, FUN = is.numeric, FUN.VALUE = c(FALSE)))
  areAllParamFun <- all(vapply(X = param, FUN = function(x){class(x) == "ModelFunction"}, FUN.VALUE = c(FALSE)))
  areAllParamChar <- all(vapply(X = param, FUN = function(x){class(x) == "character"}, FUN.VALUE = c(FALSE)))
  if(areAllParamNum){
    # do nothing for param
    new("ModelFunction", name = name, fun = fun, param = param )
    
  }else if(areAllParamFun){
    # perform sampling in the passed Functions acting as priors
    paramNames <- names(formals(fun))[-1]
    sampledParam <- mapply(FUN = function(paramNames, param){ applyFunction(object = param, xval = 1)} ,
                           paramNames,
                           param,
                           USE.NAMES = TRUE,
                           SIMPLIFY = FALSE)
    new("ModelFunction", name = name, fun = fun, param = sampledParam)
  }else if (areAllParamChar){
    new("ModelFunction", name = name, fun = fun, param = param )
  }else if(!areAllParamFun && !areAllParamNum && !areAllParamChar){
    stop("In ModelFunction Class, param argument has to be a list of numeric or characters or a list of ModelFunction class")
  }
}

setMethod("show", "ModelFunction",
          function(object){
            ToStream(object)
          })

setMethod(f="ToStream",
          signature = "ModelFunction",
          definition = function(object){        
            cat("ModelFunction", object@name, "with parameters","\t")
            pnames <- lapply(X = 1:length(object@param),
                             FUN = function(x, l){ paste(names(l)[x], "=", l[x], sep ="")},
                             l = object@param)
            
            for(i in 1:length(pnames)) cat(pnames[[i]], "\t" )
          })

setMethod(f="getParameters",
          signature = "ModelFunction",
          definition = function(object){
            sapply(X = 1:length(object@param),
                   FUN = function(x, object){
                     paste(object@name, ".", names(object@param)[[x]], " = ", object@param[[x]], sep ="")
                   },
                   object = object
                   )
          })

setMethod(f="applyFunction",
          signature = "ModelFunction",
          definition = function(object, xval){
            allArgs <- c(list(x = xval), object@param)
            y <- do.call(what = object@fun, args = allArgs)
            return(y)
          })