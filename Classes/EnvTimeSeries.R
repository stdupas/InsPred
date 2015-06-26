setClass("EnvTimeSeries",
         representation(values = "RasterStackBrickTS"),
         prototype(values = plot(rts(stack(raster(matrix(1))),as.Date(0)))),
         contains = "Environnement"
         )

setMethod("show", 
          signature="EnvTimeSeries",
          function(object){
            ToStream(object)
          })

setMethod("myPlot", 
          signature="EnvTimeSeries",
          function(object){
          plot(object@values)
          })

setMethod("ToStream", "EnvTimeSeries",
          function(object){
            cat("class      : EnvTimeSeries\nvalues: \n")
            show(object@values)
          })

setMethod(f="getValues",
          signature = "EnvTimeSeries",
          definition = function(object){
            return(object@values)
          })

envtimeseries <- function(x=list(matrix(NA),as.Date(1))) 
{
  if (!(class(x)%in%c("list","RasterStackTS","NetCDF"))) stop("wrong arguments")
  if ((class(x)=="list")&(length(x)!=2)) stop("list does not have 2 arguments") else {
    if (!(class(x[[1]])%in%c("matrix","list","RasterStackTS","RasterStack"))) stop("first argument in the list is not a matrix, a list or a RasterStackTS")
    if (class(x[[2]])!="Date") stop("second argument in the list is not a date")
    if (!((class(x[[1]])=="matrix")&(length(x[[2]])==1)|(length(x[[1]])==length(x[[2]])))) stop("length of the first and second arguments of the \n list do not correspond")
  }
  if (class(x)=="list"){
    if (class(x[[1]])=="matrix") x[[1]] <- stack(raster(x[[1]]))
    if (class(x[[1]])=="list"){
      if (all(lapply(x,FUN=class)=="matrix")) {
        x[[1]] <- stack(lapply(x,FUN=raster))
      }
    }
    x <- rts(x[[1]],x[[2]])
  }
  if (class(x)=="NetCDF"){
    Dates <-   strsplit(ncFiles[1],"_")[[1]][length(strsplit(ncFiles[1],"_")[[1]])]
    Dates <- strsplit(strsplit(Dates,"[.]")[[1]][1],"-")[[1]]
    Dates <- data.frame(Annees = substr(Dates,1,4),mois = substr(Dates,5,6),jours = substr(Dates,7,8))
    Dates <- transform(Dates, Date = paste(Annees, mois, jours, sep='/'))[-c(1:3)]
    Dates <- as.Date(as.Date(as.character(Dates[1,1])):as.Date(as.character(Dates[2,1])),origin="1970-01-01")
    print("================ 3 =================")
    
    longitude <- var.get.nc(nc,'longitude')
    latitude <- var.get.nc(nc,'latitude')
    print("================ 4 =================")
    
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
    
    nCell <- ncell(rasterAgg) #length(latitude)*length(longitude)/(aggregationParam^2)
    EnvData <- array(NA,
                     dim=c(nCell,length(Dates),length(ncFiles)),
                     dimnames=list(1:nCell,as.character(Dates),1:length(ncFiles)))
    rasterStackAgg <- stack(rasterAgg)
    layers <- NA
    print("================ 5 =================")
    
    for (ncFile in ncFiles)#ncFile=ncFiles[1]
    {
      print("================ fisrt for =================")
      
      variable = dimnames(EnvData)[[3]][which(ncFiles==ncFile)] = layers[which(ncFiles==ncFile)] = strsplit(ncFile,"_")[[1]][1]
      print("================ 6 =================")
      
      nc=open.nc(paste(ncDirectory,ncFile,sep=""))
      print("================ 7 =================")
      print(variable)
      enVarNC <- var.get.nc(nc,variable)
      print("================ 8 =================")
      
      cropBeforeAggregate <- any((dim(rasterLayer)%%aggregationParam)[1:2]!=c(0,0))
      print("================ 9 =================")
      
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
        EnvData[,as.character(Dates[i]),variable] <- values(rasterAgg)
      }
      rasterStackAgg <- stack(rasterStackAgg,rasterAgg)
    }
    print(layers)
    print("======================")
    print(dim(layers))
    print("======================")
    print(names(rasterStackAgg))
    print(dim(rasterStackAgg))
    names(rasterStackAgg) <- layers
    list(EnvData,rasterStackAgg)
    
  }
    
new("EnvTimeSeries",values=x)
}
