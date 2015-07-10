
setGeneric(
  name = "ToStream",
  def = function(object, con, description) { return(standardGeneric("ToStream"))})

setGeneric(
  name = "myWrite",
  def = function(object, file) { return(standardGeneric("myWrite"))})

setGeneric(
  name = "getArray",
  def = function(object1,object2) { return(standardGeneric("getArray"))})

setGeneric(
  name = "getMatrix",
  def = function(object1,object2) { return(standardGeneric("getMatrix"))})

setGeneric(
  name = "getValues",
  def = function(object) { return(standardGeneric("getValues"))})

setGeneric(
  name = "getStage",
  def = function(object) { return(standardGeneric("getStage"))})

setGeneric(
  name = "mySetValues",
  def = function(object,newValues,Subset) { return(standardGeneric("mySetValues"))})

setGeneric(
  name = "myAddValues",
  def = function(object,newValues,Subset) { return(standardGeneric("myAddValues"))})

setGeneric(
  name = "etsDim",
  def = function(object) { return(standardGeneric("etsDim"))})

setGeneric(
  name = "getDay",
  def = function(object1,object2) { return(standardGeneric("getDay"))})

setGeneric(
  name = "getEnvTimeSerie",
  def = function(object1,object2) { return(standardGeneric("getEnvTimeSerie"))})

setGeneric(
  name = "getEnvDay",
  def = function(object) { return(standardGeneric("getEnvDay"))})

setGeneric(
  name = "getDates",
  def = function(object) { return(standardGeneric("getDates"))})

setGeneric(
  name = "getVarNames",
  def = function(object) { return(standardGeneric("getVarNames"))})

setGeneric(
  name = "myPlot",
  def = function(object,...) { return(standardGeneric("myPlot"))})

setGeneric(
  name = "myMean",
  def = function(object1,object2) { return(standardGeneric("myMean"))})

setGeneric(
  name = "myPlus",
  def = function(object1,object2) { return(standardGeneric("myPlus"))})
setGeneric(
  name = "myMoins",
  def = function(object1,object2) { return(standardGeneric("myMoins"))})

setGeneric(
  name = "getParameters",
  def = function(object) { return(standardGeneric("getParameters"))})

setGeneric(
  name = "applyFunction",
  def = function(object, xval) { return(standardGeneric("applyFunction"))})

setGeneric(
  name = "applyModel",
  def = function(object) { return(standardGeneric("applyModel"))})

setGeneric(
  name = "computeDistanceMatrix",
  def = function(object, con, description) { return(standardGeneric("computeDistanceMatrix"))})
