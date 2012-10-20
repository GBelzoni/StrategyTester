#Trade Classes - Requires MarketDataClasses
source("R/MarketDataClasses.R")

#Class -Trade
  
  #Data members for Trade Name, Notional, OtherInfo
  #Should set initialiser to error if wring info  
  Trade = function(Name,Type,Notional)
  {
    rtrn = list()
    class(rtrn)="Trade"
    rtrn$Name = Name
    rtrn$Type = Type
    rtrn$Notional = Notional
    return(rtrn)
  }

  
  #This is the method which needs to be made polymorphic for options etc
  Price <- function(object,...) UseMethod("Price") #You have to put this random line in when you first define polymorpic function
  Price.Trade <- function(object,MarketDataSlice)
  {
    if( object$Type == "Cash") {
        return(1)
      } else if(object$Type == "Eq") {
        return(as.numeric(coredata(Cl(MarketDataSlice$Data))))  
      } else {
        cat('object Type Not Recognised \n')
        NULL
      } 
  }
    
  
  Value <- function(object,...) UseMethod("Value")#You have to put this random line in when you first define polymorpic function
  Value.Trade = function(Trade,MarketDataSlice){
              Price(Trade,MarketDataSlice)*Trade$Notional}
  

  #Test Trade Class
  T1 = Trade("Cash","Cash",100)
  T2 = Trade("Eq1","Eq",100)
  TradeList = list(T1,T2)  
  Price(T1)#DSlice1)
  (Value(T2,MDSlice1))
