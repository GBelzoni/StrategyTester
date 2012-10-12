#Need Trade Classes
source("TradeClasses.R")
##Class - Porfolio
  #Data member for portfolio name, and tradenames
  setClass("Portfolio",
           representation(
             PortfolioName = "character",
             Trades = "list"
             )
  )
  Portfolio = function(PortfolioName_,Trades_){
    new(Class="Portfolio",
        PortfolioName = PortfolioName_,
        Trades=Trades_
        )}

  setGenericVerif(x="addTrade",y  <- function(object,Trade){standardGeneric("addTrade")})
  #removeGeneric("Value")
  setMethod("addTrade","Portfolio", 
            function(object,Trade){ 
              object@Trades = c(object@Trades, Trade) #Add Trade to Trade list
              return(object)
  }
  )

  #Test Portfolio Class
  P1=Portfolio("P1",TradeList)
  P1@PortfolioName
  P1@Trades
  T3 = Trade("T3","Eq",200)
  P1=addTrade(P1,T3)
  
  
#Class - PortfolioSlice 
  #Data MarketData, portfolio
  #Member for Price Info, and Value
  #Member for diagnostics - ie greeks
  setClass("PortfolioSlice",
           representation(
             MarketData = "MarketDataSlide",
             Portfolio = "Portfolio")
  )
  #Initializer function
  PortfolioSlice = function(Portfolio_,MarketData_){
      new(Class="PortfolioSlice",
          Portfolio=Portfolio_,
          MarketData = MarketData_)}
  
  #PricingMethod
  setGenericVerif(x="Price",y  <- function(object){standardGeneric("Price")})
  #removeGeneric("Value")
  setMethod("Price","PortfolioSlice", 
            function(object,MarketDataSlide){ 
              PriceVec = numeric(length(object@Portfolio@Trades))
              PriceVec=sapply(object@Portfolio@Trades,Price, object@MarketData)
              names(PriceVec)=sapply(object@Portfolio@Trades,getNames)
              return(PriceVec)
  }
  )
  #ValueMethod
  setGenericVerif(x="Value",y  <- function(object,PricingData){standardGeneric("Value")})

  setMethod("Value","PortfolioSlice",
            function(object,MarketDataSlide){
              Values = sapply(object@Portfolio@Trades, Value, object@MarketData)             
              names(Values)=sapply(object@Portfolio@Trades,getNames)
              return(Values)}
  )

#  #Test
  P1Slide = PortfolioSlice(P1,MDSlide1)
  Price(P1Slide)
  (Value(P1Slide)) 

