setwd("/home/egce/Bureau/Ludivine/forwardKenya/")
setwd("/home/dupas/forwardKenya/")
library(RNetCDF) # for nc2EnvDataAndRasterStack function (readNetCDF.R script), to read NetCDF formatted data
library(raster) # library to manage raster format data 
library(rts)


# Read RnetCDF data
source("dataHandling.R") # functions to handle data (read netCDF, create burning period, aggregate tables)
source("inference.R") # calculates and sample posterior using data and prior information
source("dispersionFunctions.R") # calculates and sample posterior using data and prior information
source("NicheFunctions.R") # calculates and sample posterior using data and prior information
source("Classes/ModelFunction.R")
EnvDataRasterStack = nc2EnvDataAndRasterStack(ncDirectory="../dataForwardKenya/",aggregationParam=1)
saveRDS(EnvDataRasterStack,"../dataForwardKenya/ObjectEnvdataRasterStackAggr1_1998_2003")

#Lecture des données déjà aggrégées. Gain de temps
#EnvDataRasterStack = readRDS("../dataForwardKenya/ObjectEnvdataRasterStackAggr1_1998_2003")

rasterStack <- EnvDataRasterStack[[2]]
EnvData <- EnvDataRasterStack[[1]]
Dimnames <- dimnames(EnvData); Dimnames[[3]] <- append(Dimnames[[3]],"Tmean")
EnvData <- array(EnvData,dim=c(length(Dimnames[[1]]),length(Dimnames[[2]]),length(Dimnames[[3]])),dimnames=Dimnames)
EnvData[,,c("Tmin","Tmax")] <- EnvData[,,c("Tmin","Tmax")] - 273.15
EnvData[,,c("Tmean")] <- (EnvData[,,"Tmin"]+ EnvData[,,"Tmax"])/2
rm(EnvDataRasterStack)

####### release data (one individual per cell between 1998/01/01 and 1998/05/31)

# number of days averaged to calculate maize health
nDaysAveraged = 10
# number of days for burning period
burningPeriodDuration = 150

stopBurningDate=colnames(EnvData)[burningPeriodDuration]
startComputingDate=colnames(EnvData)[nDaysAveraged]
maxStopComputingDate <- as.Date("2003-12-31")

#Dates <- as.Date(as.Date(minDates):as.Date(maxDate),origin="1970/01/01")
#release <- releaseFill(rasterStack,startDate=colnames(EnvData)[nDaysAveraged],stopDate="1998/05/31") # remove releaseFill if useless

## Recovery, reading and formating for analysis (colnames, and temporal range)
recovery <- formatAbundanceData(read.table("../dataForwardKenya/Stemborer_Kenya2001_2005.csv"),
                                col_names=c(x="Long_dec",y="Lat_dec",birthDate="Diss_Date",size="B._fusca",sampling_effort="no._plants"),
                                maxDate=maxStopComputingDate)

dim(recovery)
stopComputingDate = min(max(recovery$birthDate),max(as.Date(colnames(EnvData))))

parentSize <- parentSizeFill(rasterStack,
                             EnvData,
                             startComputingDate=startComputingDate,
                             stopBurningDate=stopBurningDate,
                             stopComputingDate=stopComputingDate)

#Calcul de la distance en km entre deux demes à partir de leurs coordonnées

distMat <- distanceMatrixFromRaster2(object = rasterStack)

# on calcule la moyenne sur les nDaysAveraged précedents jours
EnvDataAveraged <- computeMeanEnvData(EnvData, vars=c("Rainf","Tmin","Tmax"), nDaysAveraged)

#On récupère les dates d'interêt seulement.
EnvData2 <- EnvData[,colnames(parentSize),]
Tmean <- (EnvData2[,,2]+EnvData2[,,3])/2 -273.15
DevRateEgg <- developmentRateLogan(Tmean,species="Bf",life_stage="Egg")
DevRateLar <- developmentRateLogan(Tmean,species="Bf",life_stage="phyloLarvae")
DevRateLar <- developmentRateLogan(Tmean,species="Bf",life_stage="stemLarvae")
DevRatePup <- developmentRateLogan(Tmean,species="Bf",life_stage="Pupae")


EnvDataAveraged <- EnvDataAveraged[,colnames(parentSize),]

# Model Implementation

r.Rainf.Xmin.prior <- Function(fun = uniform, param = list(min = 0, max = 2))
r.Rainf.Xmax.prior <- Function(fun = uniform, param = list(min = 2, max = 10))
r.Rainf.Yopt.prior <- Function(fun = uniform, param = list(min = 5, max = 50))
r.Rainf.Xopt.prior <- Function(fun = uniform, param = list(min = 0, max = 10))
r.Rainf.Niche <- model(varEnv = EnvDataAveraged, 
                       fun = Function(fun = conquadraticSkewed1, 
                                      param = list(r.Rainf.Xmin.prior, 
                                                   r.Rainf.Xmax.prior,
                                                   r.Rainf.Yopt.prior,
                                                   r.Rainf.Xopt.prior)))

r.Rainf.Niche <- Function(fun = conquadraticSkewed1, param = list(x, 
                                                                  Xmin=r.Rainf.Xmin.prior, 
                                                                  Xmax=r.Rainf.Xmax.prior, 
                                                                  Xopt=r.Rainf.Xopt.prior, 
                                                                  Yopt=r.Rainf.Yopt.prior, 
                                                                  ))
