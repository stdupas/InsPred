setClass("EcoModel",
         representation(ecoDay="EcoDay"),
         prototype(varName = "Rainf",
                   fun = function(x, a=1, b=2) a*x^2+b,
                   stages = "Egg",
                   submodel = list(a=1,b=2),
                   supermodel = FALSE,
                   ecoDay = EcoDay(list(values=array(0,dim=list(1,1,1)),
                                                 dates=as.Date("1998-01-01"),
                                                 envDay=brick(raster(matrix(1))),
                                                 stages="Egg",
                                                 variables="layer"))),
                   
         contains = "model",
         validity = function(object) {
           if (!(object@varName %in% getVarNames(object@ecoDay))) stop("some eco-variables (varName) are missing in ecoDay")
           return (TRUE)
         }
)

setMethod(
  f = "applyModel", 
  signature = "EcoModel",
  definition = function(object){
    return(applyModel(object,object@ecoDay))
  })

setMethod(
  f = "ToStream",
  signature = "EcoModel",
  definition = function(object){
    cat ("slot 'varName' " , object@varName, "\n")
    cat("slot 'fun'  ") 
    show(object@fun) 
    cat("slot 'stages' ",object@stages,"\n") 
    cat("slot 'submodel' \n")
    show(object@submodel)
    cat("slot 'supermodel' ", object@supermodel, "\n\n")
    cat("slot 'ecoDay' \n")
    ToStream(object@ecoDay)

  }
)











