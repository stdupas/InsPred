
setGeneric(
  name = "ToStream",
  def = function(object, con, description) { return(standardGeneric("ToStream"))})

setGeneric(
  name = "myWrite",
  def = function(object, file) { return(standardGeneric("myWrite"))})

setGeneric(
  name = "getValues",
  def = function(object) { return(standardGeneric("getValues"))})

setGeneric(
  name = "etsDim",
  def = function(object) { return(standardGeneric("etsDim"))})

setGeneric(
  name = "getEnvTimeSeries",
  def = function(object) { return(standardGeneric("getEnvTimeSeries"))})

setGeneric(
  name = "getEcolArray",
  def = function(object) { return(standardGeneric("getEcolArray"))})

setGeneric(
  name = "getDates",
  def = function(object) { return(standardGeneric("getDates"))})

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
