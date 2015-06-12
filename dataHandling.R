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

formatAbundanceData <- function(recovery,col_names=c(x="Long_dec",y="Lat_dec",birthDate="Diss_Date",size="B._fusca",sampling_effort="no._plants"),maxDate="2003/12/31")
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

computeMeanEnvData = function(data, var, nbDays) {
  dataOld = data
  for(d in 1:length(data[,1,var])) {
    for(i in (nbDays+1):length(data[d,,var])) {
      m = mean(dataOld[d,(i-nbDays):(i-1),var])
      data[d,i,var] = m
    }
  }
  return(data)
}


aggregateDays = function(data, nbDays, Dates) {
  nbDates = round(length(Dates)/nbDays)-1
  dataNew = array(0, dim=c(dim(data)[1],nbDates,dim(data)[3]))
  # pour chaque couche
  for(c in 1:dim(data)[3]) {
    # pour chaque deme
    for(d in 1:dim(data)[1]) {
      j = 1
      # pour toutes les nbDays dates
      for(i in seq(nbDays,length(Dates),by=nbDays)) {
        m = mean(data[d,(i-nbDays+1):i,c])
        dataNew[d,j,c] = m
        j = j+1
      }
    }
  }
  dimnames(dataNew)[1] = dimnames(data)[1]
  dimnames(dataNew)[2] = dimnames(data[,seq(nbDays,length(Dates),by=nbDays),])[2]
  dimnames(dataNew)[3] = dimnames(data)[3]
  
  return(dataNew)
}
