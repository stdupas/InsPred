setwd("/home/dupas/forwardKenya/")
library(RNetCDF) # for nc2EnvDataAndRasterStack function (readNetCDF.R script), to read NetCDF formatted data
library(raster) # library to manage raster format data 



# Read RnetCDF data
source("dataHandling.R") # functions to handle data (read netCDF, create burning period, aggregate tables)

EnvDataRasterStack = nc2EnvDataAndRasterStack(ncDirectory="../dataForwardKenya/",aggregationParam=1)
saveRDS(EnvDataRasterStack,"../dataForwardKenya/ObjectEnvdataRasterStackAggr1_1998_2003")

#Lecture des données déjà aggrégées. Gain de temps
# EnvDataRasterStack = readRDS("../dataForwardKenya/ObjectEnvdataRasterStackAggr1_1998_2003")

rasterStack <- EnvDataRasterStack[[2]]
EnvData <- EnvDataRasterStack[[1]]
rm(EnvDataRasterStack)

####### release data (one individual per cell between 1998/01/01 and 1998/05/31)

# number of days averaged to calculate maize health
nDaysAveraged = 10

burningPeriodDuration = 150

## Recovery, reading and formating for analysis (colnames, and temporal range)

recovery <- read.table("../dataForwardKenya/Stemborer_Kenya2001_2005.csv")
recovery <- formatAbundanceData(recovery,col_names=c(x="Long_dec",y="Lat_dec",birthDate="Diss_Date",size="B._fusca",sampling_effort="no._plants"),maxDate="2003/12/31")
dim(recovery)

maxDates = min(max(recovery$birthDate),max(as.Date(colnames(EnvData))))
#Dates <- as.Date(as.Date(minDates):as.Date(maxDates),origin="1970/01/01")
#release <- releaseFill(rasterStack,startDate=colnames(EnvData)[nDaysAveraged],stopDate="1998/05/31") # remove releaseFill if useless
parentSize <- parentSizeFill(rasterStack,startDate=colnames(EnvData)[nDaysAveraged],stopDate=colnames(EnvData)[burningPeriodDuration],maxDates=maxDates)
recoverySize <- 

#Calcul de la distance en km entre deux demes à partir de leurs coordonées

distMat <- distanceMatrixFromRaster2(object = rasterStack)

