#Trade Strategy classes
source("PortfolioClasses.R")
  #TradeStrategy
  #Data - MDobject, Portfolio, time, PL, PortfolioTS
  #Methods - initialise constructor, updatePort, stepStrat, runStrat 
  setClass("TradeStrategy",
             representation(
               MarketData = "MarketData",
               Portfolio = "Portfolio",
               CurrentTime = "integer", ##Hmm, have to make this adapt for different xts classes
               Results = "data.frame"
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
      Results = data.frame()
      return(.Object)
      }
    
    )
  
  #need to define generic method before we can define class method
  setGenericVerif(x="TradeStrategy",y  <- function(object){standardGeneric("TradeStrategy")})
  TradeStrategy  <- function(MarketData_,Portfolio_){
    new(Class="TradeStrategy",MarketData=MarketData_,Portfolio=Portfolio_)
  }

  #Accessors
  setGenericVerif(x="getCT",y  <- function(object){standardGeneric("getCT")})
  setMethod("getCT","TradeStrategy",function(object){return(object@CurrentTime)})
  setGenericVerif(x="getData",y  <- function(object){standardGeneric("getData")})
  setMethod("getData","
            TradeStrategy",function(object){return(object@MarketData@Data)})
  setGenericVerif(x="getPortfolio",y  <- function(object){standardGeneric("getPortfolio")})
  setMethod("getPortfolio","TradeStrategy",function(object){return(object@Portfolio)})



  #updatePortfolio implementation
  #This function defines trade strategy. 
  #It tells how to update notionals given MD and current portfolio
  setGenericVerif(x="updSignal",y  <- function(object,time){standardGeneric("updSignal")})
  
  #THis is for MA crossover
  setMethod("updSignal","TradeStrategy", 
            function(object,time){ 
              Data = getMD(object)
              #Previous Moving average vals
              MAs0 = Data[(time-1),"MAs"]
              MAl0 = Data[(time-1),"MAl"]
              #Current MA vals
              MAs1 = Data[(time),"MAs"]
              MAl1 = Data[(time),"MAl"]
            
              
              if(
              
              
              
  )


  
  #updatePortfolio implementation
  #This function defines trade strategy. It tells how to update notionals TradeStrategy MD

  

  #Te  
  #st and current portfolio
  
  TS1=TradeStrategy(MD1,P1)
  TS1

  