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
  MarketData = function(Data_){new(Class="MarketData",Data=Data_)} #Constructor
  
  #Test MarketData
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
  setClass("MarketDataSlide",
      representation(
        TimeIndex = "numeric", 
        TimeClass = "character",
        Data = "xts"
        )
  )

  #Methods: GetData, UpdateData, currentDate, subsetData
  MarketDataSlide = function(MarketData_, TimeIndex_){
    new(Class="MarketDataSlide",
        TimeIndex = TimeIndex_,
        TimeClass = "Date", #Have to fix to read POSIX date class
        Data = MarketData_@Data[TimeIndex_]
        )}
  
  #Test MarketData
  MDSlide1 = MarketDataSlide(MD1,1)


#Class -Trade
  
  #Data members for Trade Name, Notional, OtherInfo
  #Should set initialiser to error if wring info  
  setClass("Trade",
           representation(
             TradeName = "character",
             TradeType = "character",
             Notional = "numeric"            
             )
  )
  #Initialiser function
  Trade = function(Name_,Type_,Notional_){
            new(Class="Trade",
                TradeName = Name_,
                TradeType = Type_, 
                Notional = Notional_)}
  #Acessor
  setGenericVerif(x="getNotional",y  <- function(object){standardGeneric("getNotional")})
  setMethod("getNotional","Trade",function(object){return(object@Notional)}
  )


  setGenericVerif(x="getNames",y  <- function(object){standardGeneric("getNames")})
  setMethod("getNames","Trade",function(object){return(object@TradeName)}
  )  #PricingMethod
  setGenericVerif(x="Price",y  <- function(object,MarketDataSlide){standardGeneric("Price")})
  #removeGeneric("Value")
  
  #This is the method which needs to be made polymorphic for options etc
  setMethod("Price","Trade", function(object,MarketDataSlide) { 
      if( object@TradeType == "Cash") {
        1
      } else if(object@TradeType == "Eq") {
        as.numeric(coredata(Cl(MarketDataSlide@Data)))  
      } else {
        cat('Trade Type Not Recognised \n')
        NULL
      } 
    
    })
  #ValueMethod
  setGenericVerif(x="Value",y  <- function(object,MarketDataSlide){standardGeneric("Value")})
  setMethod("Value","Trade", 
            function(object,MarketDataSlide){
              Price(object,MarketDataSlide)*object@Notional}
  )

  #Test Trade Class
  T1 = Trade("Cash","Cash",100)
  T2 = Trade("Eq1","Eq",100)
  getNotional(T2)
  TradeList = c(T1,T2)  
  (Price(T2,MDSlide1))
  (Value(T1,MDSlide1))

#Class - Porfolio
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
  
  #Test Portfolio Class
  P1=Portfolio("P1",TradeList)
  P1@PortfolioName
  P1@Trades

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
  setGenericVerif(x="TradeStrategy",y  <- function(object){standardGeneric("Value")})
  TradeStrategy  <- function(MarketData_,Portfolio_){
    new(Class="TradeStrategy",MarketData=MarketData_,Portfolio=Portfolio_)
  }
  #Test
  TS1=TradeStrategy(MD1,P1)
  TS1

  