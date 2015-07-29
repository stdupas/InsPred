setwd("/Users/jean-francoissilvain/Documents/marc-fred/forwardKenya")
setwd("/home/egce/Bureau/Ludivine/forwardKenya/")
setwd("/home/dupas/forwardKenya/")
library(RNetCDF) # for nc2EnvDataAndRasterStack function (readNetCDF.R script), to read NetCDF formatted data
library(raster) # library to manage raster format data 
library(rts)


# Read RnetCDF data
#source("dataHandling.R") # functions to handle data (read netCDF, create burning period, aggregate tables)
source("inference.R") # calculates and sample posterior using data and prior information
source("dispersionFunctions.R") # calculates and sample posterior using data and prior information
source("NicheFunctions.R") # calculates and sample posterior using data and prior information
source("Classes/Generics.R")
#source("Classes/AbstractModel.R")
#source("Classes/ModelFunction.R")
#source("Classes/Environment.R")
source("Classes/EnvTimeSerie.R")
source("Classes/EnvTimeSeries.R")
source("Classes/ageClassTransition.R")
source("Classes/EcoDay.R")
source("Classes/model.R")
source("Classes/EcoModel.R")

burnin_period = 100

# Load environmental data
Rainf <- EnvTimeSerie("../dataForwardKenya/Rainf_WFDEI_19980101-20031231.nc")
Tmin <-  myPlus(EnvTimeSerie("../dataForwardKenya/Tmin_WFDEI_19980101-20031231.nc"),-273.15)
Tmax <-  myPlus(EnvTimeSerie("../dataForwardKenya/Tmax_WFDEI_19980101-20031231.nc"),-273.15)
Tmean <-  myPlus(EnvTimeSerie("../dataForwardKenya/Tmean_WFDEI_19980101-20031231.nc"),-273.15)
# list them
EnvData <- EnvTimeSeries(list(Rainf,Tmin,Tmax,Tmean))

# 
PlantQ <- plantQuality(EnvData,"Rainf",15)
EnvData <- EnvTimeSeries(list(Rainf,Tmin,Tmax,Tmean,PlantQ))
plot(getDay(EnvData,1))
#bidon <- EnvTimeSerie(list(crop(getValues(Tmean),extent(getValues(Tmean))-2),getDates(Tmean),"Rainf"))
#EnvData <- EnvTimeSeries(list(Rainf,bidon))
stages <- rep(c("Egg","PhyloL","StembL","Pupae","Adult"),each=10)
ED <- EcoDay(list(array(0,dim = c(dim(getValues(Rainf))[1:2],5*10)),
                  
                  as.Date("2001-01-01"),
                  getDay(EnvData,"2001-01-01"),
                  stages=rep(c("Egg","PhyloL","StembL","Pupae","Adult"),each=10),
                  variables=names(getDay(EnvData,"2001-01-01")))
             )

ED1 <- mySetValues(ED,1,"Adult")
distanceMatrix <- distanceMatrixFromRaster2(getEnvDay(ED))
landDim <- dim(getArray(ED,NULL))[1:2]
getDay(EnvData,1)
a <- as.Date(getDates(EnvData))
ACtransition <- readRDS("data/ACtranstion")

ACtransition <- ageClassTransition(Tmean,developmentRateLogan, stages,getDates(Tmean)[1:2],"Bf")

ageClassTransition_numeric(devEnvVar=28,developmentRateLogan,"Bf",stages,c(Adult=10,Egg=10, PhyloL=10,  Pupae=10, StembL=10 ))

function(x) 
{
  # Arguments: a list
  # - [[1]] an array class variables [Age_class,X,Y,Stage]
  # - [[2]] a one day Date class variable 
  # - [[3]] an EnvTimeSerie class (X,Y dimension should correspond with the array)
  new("EcolData",ecolarray=x[[1]],dates=x[[2]],envtimeserie=x[[3]])
}  
devRateLogan <- developmentRateLogan(getValues(Tmean),"Bf","Egg")



devRateBfEggFunction <- ModelFunction(name="devRateBfEgg", fun=developmentRateLogan, param=list("Bf","Egg"))
  
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

#######################################################
##############Fontion en 49 est meilleure##############
#######################################################
#On récupère les dates d'interêt seulement.
#EnvData2 <- EnvData[,colnames(parentSize),]
#Tmean <- (EnvData2[,,2]+EnvData2[,,3])/2 -273.15
#DevRateEgg <- developmentRateLogan(Tmean,species="Bf",life_stage="Egg")
#DevRateLar <- developmentRateLogan(Tmean,species="Bf",life_stage="phyloLarvae")
#DevRateLar <- developmentRateLogan(Tmean,species="Bf",life_stage="stemLarvae")
#DevRatePup <- developmentRateLogan(Tmean,species="Bf",life_stage="Pupae")


EnvDataAveraged <- EnvDataAveraged[,colnames(parentSize),]

# Model Implementation

r.Rainf.Xmin.prior <- ModelFunction(fun = runif, param = list(min = 0, max = 2))
r.Rainf.Xmax.prior <- ModelFunction(fun = uniform, param = list(min = 2, max = 10))
r.Rainf.Yopt.prior <- ModelFunction(fun = uniform, param = list(min = 5, max = 50))
r.Rainf.Xopt.prior <- ModelFunction(fun = uniform, param = list(min = 0, max = 10))
r.Rainf.Niche <- model(varEnv = EnvDataAveraged, 
                       fun = ModelFunction(fun = conquadraticSkewed1, 
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

