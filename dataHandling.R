nc2EnvDataAndRasterStack <- function(ncDirectory=paste(wd,"ForwardSimulData/",sep=""),aggregationParam=40)
{
  print("================ 1 =================")
  ncFiles=grep(".nc",list.files(ncDirectory),value=TRUE)
  nc=open.nc(paste(ncDirectory,ncFiles[1],sep=""))
  print("================ 2 =================")
  
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

# try to remove this function if not used (we use parentSizeFill instead)
releaseFill <- function(rasterStack,startDate,stopDate)
{
  release=as.data.frame(xyFromCell(rasterStack,1:ncell(rasterStack)))
  birthDates <- as.Date(as.Date(startDate):as.Date(stopDate),origin="1970/01/01")
  release <- release[rep(1:nrow(release),length(birthDates)),]
  release$birthDate=rep(birthDates,each=ncell(rasterStack))
  release$size=1
  release$demeNb <-  cellFromXY(object = rasterStack, xy = release[, c("x", "y")])
  release
}

parentSizeFill <- function(rasterStack,EnvData,startComputingDate,stopBurningDate,stopComputingDate)
{
  parentSizeDates <- as.Date(as.Date(startComputingDate):as.Date(stopComputingDate),origin="1970-01-01")
  parentSize=array(0,dim=c(ncell(rasterStack),length(parentSizeDates)),dimnames=list(1:ncell(rasterStack),as.character(parentSizeDates)))
  parentSize[,as.character(as.Date(as.Date(startComputingDate):as.Date(stopBurningDate),origin="1970-01-01"))] <- 1
  parentSize
}

# to be removed (not useful at the moment) not finished
recoverySizeFill <- function(recoveryfile = read.table("../dataForwardKenya/Stemborer_Kenya2001_2005.csv"),
                             col_names = c(x="Long_dec",y="Lat_dec",birthDate="Diss_Date",size="B._fusca",sampling_effort="no._plants"),
                             startDate,
                             maxDate="2003/12/31"
)
{
  recovery <- formatAbundanceData(Datarecoveryfile,col_names,maxDate)
  recoverySizeDates <- as.Date(as.Date(startDate):as.Date(maxDate),origin="1970-01-01")
  parentSize=array(0,dim=c(ncell(rasterStack),length(parentSizeDates)),dimnames=list(1:ncell(rasterStack),as.character(parentSizeDates)))
  parentSize[,as.character(as.Date(as.Date(startDate):as.Date(stopDate),origin="1970-01-01"))] <- 1
  parentSize
}

aggregation <- function(df,BY,methodes)
{
  # Function to aggregate lines of a data.frame
  # arguments
  # df: data.frame to aggregate
  # BY: columns to use for aggregation of lines, lines having the same value in all the columns will be aggregated
  # value
  # a data.frame aggregated. Numeric and integer columns are given mean value of aggregated lines. Character and other class
  # of vectors are given collapsed character string value, or first value if all the same
  
  df$MergeBY <- NA
  for (ligne in 1:nrow(df)){df$MergeBY[ligne]=paste(df[ligne,BY],collapse="_")}
  df <- df[order(df[,"MergeBY"]),];rownames(df)=1:nrow(df)
  df_return=df[1,];df_return=df_return[-1,]
  ligne=1
  methodes = append(methodes,"Name")
  while (ligne <= dim(df)[1])
  {
    print(ligne)
    lignes = which(df[,"MergeBY"]==df[ligne,"MergeBY"])
    if (length(lignes)==0) {ligne=ligne+1} else{
      sub_df <- df[lignes,]
      if (length(lignes)>1) {df_return <- rbind(df_return,sub_df[1,])
                             for (colonne in 1:dim(sub_df)[2]) {
                               df_return[nrow(df_return),colonne] = switch(methodes[colonne],
                                                                           Mean = mean(na.omit(sub_df[,colonne])),
                                                                           Sum =  sum(na.omit(sub_df[,colonne])),
                                                                           Name = na.omit(sub_df[,colonne])[1],
                                                                           Paste = paste(sub_df[,colonne],collapse="_")
                               )
                             } 
      }
      else {df_return <- rbind(df_return,df[lignes,])}
      ligne=ligne+length(lignes)
    }
  }
  return(df_return[,-ncol(df_return)])
}

formatAbundanceData <- function(recovery,
                                col_names=c(x="Long_dec",y="Lat_dec",birthDate="Diss_Date",size="B._fusca",sampling_effort="no._plants"),
                                maxDate="2003/12/31")
{
  recovery <- recovery[,col_names];dim(recovery)
  colnames(recovery) <- names(col_names)
  recovery = recovery[-which(is.na(recovery$sampling_effort)),];dim(recovery)
  #recovery$B._fusca = as.numeric(recovery$B._fusca)/recovery$no._plants
  recovery$birthDate <- as.Date(as.character(recovery$birthDate),format="%d/%m/%y")
  recovery$demeNb <- cellFromXY(object = rasterStack, xy = recovery[, c("x", "y")])
  recovery <- aggregation(recovery,BY=c("birthDate","demeNb"),methodes=c("Mean","Mean","Name","Sum","Sum","Name"))
  recovery <- recovery[which(as.Date(recovery$birthDate)<=as.Date("2003/12/31")),]
  recovery  
}

computeMeanEnvData = function(data, vars, nbDays) {
  dataOld = data
  for(d in 1:length(data[,1,vars[1]])) {
    for(i in (nbDays+1):length(data[d,,vars[1]])) {
      for (var in vars) {
        m = mean(dataOld[d,(i-nbDays):(i-1),var])
        data[d,i,var] = m
      }
    }
  }
  return(data)
}

computeMeanEnvData = function(data, vars, nbDays) {
  dataStacked <- array(data[,nbDays:dim(EnvData)[2],],
                       dim=c(dim(data)[1],dim(data)[2]-nbDays+1,dim(data)[3],nbDays),
                       dimnames=list(dimnames(data)[[1]],dimnames(data)[[2]][nbDays:dim(data)[2]],dimnames(data)[[3]],1:nbDays))
  for (Day in 1:nbDays)
  {
    dataStacked[,,,Day] <- data[,Day:(dim(data)[2]-nbDays+Day),]
  }
  dataSum <- rowMeans(dataStacked,na.rm=TRUE,dims=3)
}


distanceMatrixFromRaster2 =
  function(object){
    # Computes a pairwise distance matrix from a raster object
    #
    # Args:
    #   object: a raster object from which computes distances
    #
    # Returns:
    #   A matrix of distances in meters if a coordinate system is precised
    
    # Extract coordinates from raster object
    coords = xyFromCell(object = object, cell = 1:ncell(object), spatial=FALSE)
    lat = coords[,1]
    lon = coords[,2]
    # Compute distance matrix
    dist = NULL
    for(i in 1:length(lat)) {
      res = NULL
      for(j in 1:length(lon)) {
        res = c(res,sphericDistance(lat[i],lat[j],lon[i],lon[j]))
      }
      dist = rbind(dist,res)
    }
    return(dist)
  }

sphericDistance <- function (lat1, lat2, lon1, lon2) 
{
  if (lat1 == lat2 & lon1 == lon2) 
    distance <- 0
  else {
    rlat1 = radian(lat1)
    rlat2 = radian(lat2)
    rlon = radian(lon2 - lon1)
    distance <- 60 * (180/pi) * acos(sin(rlat1) * sin(rlat2) + 
                                       cos(rlat1) * cos(rlat2) * cos(rlon))
    distance <- distance * 1852/1000
  }
  distance
}

radian <- function (degree) 
{
  radian <- degree * (pi/180)
  radian
}