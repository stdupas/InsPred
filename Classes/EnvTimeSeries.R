setClass("EnvTimeSeries",
         representation(values = "list"),
         prototype(values = plot(brick(raster(matrix(1))),as.Date(0))),
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

setMethod("getDay",
          signature = "EnvTimeSeries",
          function(object,date){
            return( values(getValues(lapply(getValues(EnvData)[[1]]))[,1] )
          })


EnvTimeSeries <- function(x)
{
  # Arguments: a list of EntTimeSerie class variable
  new("EnvTimeSeries",values=x)
}