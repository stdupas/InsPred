setClass("EnvTimeSerie",
         representation(values = "RasterStack",dates = "Date"),
         prototype(values = plot(rts(stack(raster(matrix(1))),as.Date(0)))),
         contains = "Environnement",
         validity = function(object){
           if (nlayers(object@values)!=length(object@dates)){
             stop("length values differ from length of dates")
           } else {}
         return(TRUE)
         }
         )

setMethod("show", 
          signature="EnvTimeSerie",
          function(object){
            ToStream(object)
          })

setMethod("myPlot", 
          signature="EnvTimeSerie",
          function(object,Layers){
          plot(object@values,Layers)
          })

setMethod("ToStream", 
          signature = "EnvTimeSerie",
          function(object){
            cat("class      : EnvTimeSerie\nvalues: \n")
            show(object@values)
          })

setMethod(f="getValues",
          signature = "EnvTimeSerie",
          definition = function(object){
            return(object@values)
          })

setMethod(f="getDates",
          signature = "EnvTimeSerie",
          definition = function(object){
            return(object@dates)
          })


setMethod("myMean",
          signature = "EnvTimeSerie",
          function(object1,object2){
          Object <- object1
          Object@values <- stack((object1@values+object2@values)/2)
          Object
          })

setMethod("myPlus",
          signature = "EnvTimeSerie",
          function(object1,object2){
            if ((class(object1)=="numeric")&(class(object2)=="EnvTimeSerie")) {
              Object <- object2
              Object@values <- stack(object1+object2@values)
            } else if ((class(object2)=="numeric")&(class(object1)=="EnvTimeSerie")) {
              Object <- object1
              Object@values <- stack(object2+object1@values)
            } else if ((class(object1)=="EnvTimeSerie")&(class(object2)=="EnvTimeSerie")){
              Object <- object1
              Object@values <- stack(object2@values+object1@values)
            } else {stop ("invalid arguments")}
            Object
          })

setMethod("myMoins",
          signature = "EnvTimeSerie",
          function(object1,object2){
            if ((class(object1)=="numeric")&(class(object2)=="EnvTimeSerie")) {
              Object <- object2
              Object@values <- stack(object1-object2@values)
            } else if ((class(object2)=="numeric")&(class(object1)=="EnvTimeSerie")) {
              Object <- object1
              Object@values <- stack(object2-object1@values)
            } else if ((class(object1)=="EnvTimeSerie")&(class(object2)=="EnvTimeSerie")){
              Object <- object1
              Object@values <- stack(object2@values-object1@values)
            } else {stop ("invalid arguments")}
            Object
          })


EnvTimeSerie <- function(x,aggregationParam=1) 
{
  if (class(x)[[1]]=="character"){
    variable <- tail(strsplit(strsplit(x[[1]],"_")[[1]][1],"/")[[1]],n=1)
    Dates <- strsplit(strsplit(tail(strsplit(x[[1]],"_")[[1]],n=1),"[.]")[[1]][1],"-")[[1]]
    Dates <- data.frame(Annees = substr(Dates,1,4),mois = substr(Dates,5,6),jours = substr(Dates,7,8))
    Dates <- transform(Dates, Date = paste(Annees, mois, jours, sep='/'))[-c(1:3)]
    Dates <- as.Date(as.Date(as.character(Dates[1,1])):as.Date(as.character(Dates[2,1])),origin="1970-01-01")
    print("====== 1 Variable and Dates done ============")
    nc=open.nc(x[[1]])
    longitude <- var.get.nc(nc,'longitude')
    latitude <- var.get.nc(nc,'latitude')
    print("=========== 2 lat lon done =======")
    pixels <- SpatialPixels(SpatialPoints(cbind(longitude,latitude)))
    rasterLayer <- raster(pixels)
    if(aggregationParam>1) {
      print("================first if =================")
      
      if (any((dim(rasterLayer)%%aggregationParam)[1:2]!=c(0,0))) {        
        cellsToRemove <- dim(rasterLayer)[1:2]%%aggregationParam 
        rasterLayer2 <- crop(rasterLayer,extent(as.vector(extent(rasterLayer))-c(0,res(rasterLayer)[1]*(cellsToRemove[1]),0,res(rasterLayer)[2]*(cellsToRemove[2]))))
      } else {
        rasterLayer2 <- rasterLayer
      }
      rasterAgg <- aggregate(rasterLayer2,aggregationParam)
    } else {
      rasterAgg <- rasterLayer
    }
    stackRasterAgg <- stack(rasterAgg)
    nCell <- ncell(rasterAgg) #length(latitude)*length(longitude)/(aggregationParam^2)
    EnvData <- array(NA,
                     dim=c(nCell,length(Dates),length(x)),
                     dimnames=list(1:nCell,as.character(Dates),1:length(x)))
    EnvData <- array(NA,
                     dim=c(nrow(rasterLayer),ncol(rasterLayer),length(Dates)),
                     dimnames=list(1:nrow(rasterLayer),1:ncol(rasterLayer),as.character(Dates)))
    #    rasterStackAgg <- stack(rasterAgg)
    enVarNC <- var.get.nc(nc,variable)
    cropBeforeAggregate <- any((dim(rasterLayer)%%aggregationParam)[1:2]!=c(0,0))
    print(length(Dates))
    for (i in 1:length(Dates))
    {
      print(i)
      values(rasterLayer) <- matrix(enVarNC[,,i],nrow=length(latitude),ncol=length(longitude),byrow=TRUE)
      if(aggregationParam>1) {
        if (cropBeforeAggregate) {
          rasterLayer2 <- crop(rasterLayer,extent(as.vector(extent(rasterLayer))-c(0,res(rasterLayer)[1]*(dim(rasterLayer)[1]%%aggregationParam),0,res(rasterLayer)[2]*(dim(rasterLayer)[2]%%aggregationParam))))
        }
        rasterAgg <- aggregate(rasterLayer2,aggregationParam)} else {rasterAgg <- rasterLayer}
      EnvData[,,i] <- matrix(values(rasterAgg),nrow=nrow(rasterAgg),ncol=ncol(rasterAgg),byrow=TRUE)
    }
    stackRasterAgg <- stack(brick(EnvData,xmn=xmin(rasterAgg),xmx=xmax(rasterAgg),ymn=ymin(rasterAgg),ymx=ymax(rasterAgg)))
    x <- list(stackRasterAgg,Dates)
  } else { if (!(class(x)%in%c("list","character"))) stop("wrong arguments")
  if ((class(x)=="list")&(length(x)!=2)) stop("list does not have 2 arguments") else {
    if (!(class(x[[1]])%in% c("matrix","list","RasterStack"))) stop("first argument in the list is not a matrix, a list or a RasterStackTS")
    if (class(x[[2]])!="Date") stop("second argument in the list is not a date")
    if (!((class(x[[1]])=="matrix")&(length(x[[2]])==1)|(nlayers(x[[1]])==length(x[[2]])))) stop("length of the first and second arguments of the list do not correspond")
  }
  if (class(x)=="list"){
    if (class(x[[1]])=="matrix") x[[1]] <- stack(raster(x[[1]]))
    if (class(x[[1]])=="list"){
      if (all(lapply(x[[1]],FUN=class)=="matrix")) {
        x[[1]] <- stack(lapply(x[[1]],FUN=raster))
      }
    }
    x <- list(x[[1]],x[[2]])
  }
  }
new("EnvTimeSerie",values=x[[1]],dates=x[[2]])
}
    

