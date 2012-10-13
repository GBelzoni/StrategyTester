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
             MarketDataSlice = "MarketDataSlice",
             Portfolio = "Portfolio")
  )
  
  #Constructor Initialise
  setMethod(
		  f="initialize",
		  signature="PortfolioSlice",
		  definition= function(.Object,Portfolio, MarketData,TimeIndex){
			  .Object@MarketDataSlice = MarketDataSlice(MarketData_ = MarketData,TimeIndex_ = TimeIndex)
			  .Object@Portfolio = Portfolio
			  #.Object@CurrentTime = (index(MarketData_@Data)[1])
			  Results = data.frame()
			  return(.Object)
		  }
  
  )
  
  #Initializer function
  PortfolioSlice = function(Portfolio_,MarketData_,TimeIndex_){
      new(Class="PortfolioSlice",
          Portfolio=Portfolio_,
          MarketData = MarketData_,
		  TimeIndex = TimeIndex_
  )}
  
  #PricingMethod
  setGenericVerif(x="Price",y  <- function(object,MarketDataSlice){standardGeneric("Price")})
  #removeGeneric("Value")
  setMethod("Price","PortfolioSlice", 
            function(object,MarketDataSlice){ 
              PriceVec = numeric(length(object@Portfolio@Trades))
              PriceVec=sapply(object@Portfolio@Trades,Price, object@MarketDataSlice)
              names(PriceVec)=sapply(object@Portfolio@Trades,getNames)
              return(PriceVec)
  }
  )
  #ValueMethod
  setGenericVerif(x="Value",y  <- function(object,PricingData){standardGeneric("Value")})

  setMethod("Value","PortfolioSlice",
            function(object,MarketDataSlide){
              Values = sapply(object@Portfolio@Trades, Value, object@MarketDataSlice)             
              names(Values)=sapply(object@Portfolio@Trades,getNames)
              return(Values)}
  )

#  #Test
  P1Slide = PortfolioSlice(Portfolio_ = P1,MarketData_ = MD1, TimeIndex_ = 1)
  Price(P1Slide)
  (Value(P1Slide)) 

