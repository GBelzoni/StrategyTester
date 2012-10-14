
##Classes for Strategy tester
##R/S class system suck btw 
##See doc in R folder to describe R/S oo weirdenss

library(xts)
library(zoo)
library(quantmod)
library(PerformanceAnalytics)

##Redefine setGeneric so that we don't overwrite existing generic functions when
##defining methods for classe
setGenericVerif <- function(x,y){if(!isGeneric(x)){setGeneric(x,y)}else{}}

#Class - Market Data
  #Data member is an xts object have to format data into this to use
  
setClass("MarketData",
      representation(
        Data = "xts"        
        )
  )
  #Methods: GetData, UpdateData, currentDate, subsetData
  MarketData = function(Data_){new(Class="MarketData",Data=Data_)} #Constructor
  
  #Test MarketData
  #Load AORD using quantmod package - All Ordinary index
  AORD = as.xts(as.zoo(read.table("../Data/AORD.csv",header=T,sep=",")))
  MD1 = MarketData(AORD)

#Class - Market Data Slide
  #Should make this base class with member to keep list of properties (char vec)
  #Then can have inherited classes with different data, OHLC Slide with corresponding prop list
  #Then Trades can inherit from base trade class and have MDrequiredparams list
  #Using these lists can call data in Pricing function to price trades
  #Should work for variety of Trade/Slide types, e.g basic equities, options slides, 
  #Even have MD generate curves object for slide and use Trade params to call this object
  #For now keep it simple

  #Data member is an xts object have to format data into this to use
  setClass("MarketDataSlice",
      representation(
        TimeIndex = "numeric", 
        TimeClass = "character",
        Data = "xts"
        )
  )

  #Methods: GetData, UpdateData, currentDate, subsetData
  MarketDataSlice = function(MarketData_, TimeIndex_){
    new(Class="MarketDataSlice",
        TimeIndex = TimeIndex_,
        TimeClass = "Date", #Have to fix to read POSIX date class
        Data = MarketData_@Data[TimeIndex_]
        )}
  
  #Test MarketData
  MDSlide1 = MarketDataSlice(MD1,1)

