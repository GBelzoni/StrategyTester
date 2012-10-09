#Need Trade Classes
source("TradeClasses.R")
#Class - PortfolioSlide 
  #Data MarketData, portfolio
  #Member for Price Info, and Value
  #Member for diagnostics - ie greeks
  setClass("PortfolioSlide",
           representation(
             MarketDataSlide = "MarketDataSlide",
             Portfolio = "Portfolio")
  )
  #Initializer function
  PortfolioSlide = function(Portfolio_,MarketDataSlide_){
      new(Class="PortfolioSlide",
          Portfolio=Portfolio_,
          MarketDataSlide = MarketDataSlide_)}
  
  #PricingMethod
  setGenericVerif(x="Price",y  <- function(object){standardGeneric("Price")})
  #removeGeneric("Value")
  setMethod("Price","PortfolioSlide", 
            function(object,MarketDataSlide){ 
              PriceVec = numeric(length(object@Portfolio@Trades))
              PriceVec=sapply(object@Portfolio@Trades,Price, object@MarketDataSlide)
              names(PriceVec)=sapply(object@Portfolio@Trades,getNames)
              return(PriceVec)
  }
  )
  #ValueMethod
  setGenericVerif(x="Value",y  <- function(object,PricingData){standardGeneric("Value")})

  setMethod("Value","PortfolioSlide",
            function(object,MarketDataSlide){
              Values = sapply(object@Portfolio@Trades, Value, object@MarketDataSlide)             
              names(Values)=sapply(object@Portfolio@Trades,getNames)
              return(Values)}
  )

#  #Test
  P1Slide = PortfolioSlide(P1,MDSlide1)
  Price(P1Slide)
  (Value(P1Slide)) 
  V2=unlist(V1)

