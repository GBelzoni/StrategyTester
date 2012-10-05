##Classes for Strategy tester
##R/S class system suck btw 
##See doc in R folder to describe R/S oo weirdenss

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
  MarketData = function(Data_)(new(Class="MarketData",Data=Data_)) #Constructor

#Class -Trad
  #Data members for Trade Name, Notional, OtherInfo
  setClass("Trade",
           representation(
             Name = "character",
             Notional = "numberic",
             OtherInfo = "list"
             )
Portfolio = function(Notionals_){new(Class="Portfolio",Notionals=Notionals_)}
#Class - Porfolio
  #Data member for portfolio Notionals
  #Member to Value portforlio
  #Member for diagnostics - ie greeks
  setClass("Portfolio",
           representation(
             Notionals = "numeric"
             )
  )
  Portfolio = function(Notionals_){new(Class="Portfolio",Notionals=Notionals_)}

  #need to define generic method before we can define class method
  setGenericVerif(x="Value",y  <- function(object){standardGeneric("Value")})
  setMethod("Value","Portfolio",
            function(object) {
              sum(object@Notionals)              
            }
  )

#TradeStrategy
  #Data - MDobject, Portfolio, time, PL, PortfolioTS
  #Methods - initialise constructor, updatePort, stepStrat, runStrat 
  setClass("TradeStrategy",
             representation(
               MarketData = "MarketData",
               Portfolio = "Portfolio",
               CurrentTime = "integer", ##Hmm, have to make this adapt for different xts classes
               PL  = "numeric",
               PortfolioTS  = "numeric"
               )
    )

  #Constructor Initialise
  setMethod(
    f="initialize",
    signature="TradeStrategy",
    definition= function(.Object,MarketData_,Portfolio_){
      .Object@MarketData = MarketData_
      .Object@Portfolio = Portfolio_
      .Object@CurrentTime = index(MarketData_)[0]
      .Object@PL = numeric(0)
      .Object@PortfolioTS = numeric(0)
      return(.Object)
      }
    
    )
  
  TradeStrategy  <- function(MD,Portfolio_){
    new(Class="TradeStrategy",MarketData=MD,Portfolio=Portfolio_)
  }
  #need to define generic method before we can define class method
  setGenericVerif(x="TradeStrategy",y  <- function(object){standardGeneric("Value")})
  setMethod("Value","Portfolio",
            function(object) {
              sum(object@Notionals)              
            }
  )
