#Trade Classes - Requires MarketDataClasses
source("MarketDataClasses.R")
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
  setMethod("getNotional","Trade",function(object){return(object@Notional)})


  setGenericVerif(x="getNames",y  <- function(object){standardGeneric("getNames")})
  setMethod("getNames","Trade",function(object){return(object@TradeName)}
  ) 
  #PricingMethod
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
