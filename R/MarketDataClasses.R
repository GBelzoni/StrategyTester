
##Classes for Strategy tester
##R/S class system suck btw 
##See doc in R folder to describe R/S oo weirdenss

library(xts)
library(zoo)
library(quantmod)
#Class - Market Data
  #Data member is an xts object have to format data into this to use

MarketData = function(Data)
{
  rtrn = list()
  class(rtrn)="MarketData"
  rtrn$Data = Data
  return(rtrn)
}

  #Test MarketData
  #Load AORD using quantmod package - All Ordinary index
  AORD = as.xts(as.zoo(read.table("Data/AORD.csv",header=T,sep=",")))
  MD1 = MarketData(AORD)

#Class - Market Data Slide
  #Should make this base class with member to keep list of properties (char vec)
  #Then can have inherited classes with different data, OHLC Slide with corresponding prop list
  #Then Trades can inherit from base trade class and have MDrequiredparams list
  #Using these lists can call data in Pricing function to price trades
  #Should work for variety of Trade/Slide types, e.g basic equities, options slides, 
  #Even have MD generate curves object for slide and use Trade params to call this object
  #For now keep it simple

MarketDataSlice = function(MarketData, TimeIndex)
{
  rtrn = list()
  class(rtrn)="MarketDataSlice"
  rtrn$TimeIndex = TimeIndex
  rtrn$TimeClass = class(index(MarketData$Data))
  rtrn$Data = MarketData$Data[TimeIndex,]
  return(rtrn)
}
  
  #Test MarketData
  MDSlice1 = MarketDataSlice(MD1,1)


