#Need Trade Classes
source("R/TradeClasses.R")
##Class - Porfolio
  #Data member for portfolio name, and tradenames
  Portfolio = function(Name,Trades){
    rtrn = list()
    class(rtrn)="Portfolio"
    rtrn$Name = Name
    rtrn$Trades = Trades
    return(rtrn)
        }

  #Need an addTrade function
  addTrade <- function(object,...) UseMethod("addTrade") #You have to put this random line in when you first define polymorpic function
  addTrade.Portfolio = function(object,Trade)
  {
    object$Trades[[length(object$Trades)+1]]=Trade
    return(object)
  }
  
  #Test Portfolio Class
  TradeList = list(T1,T2)    
  P1=Portfolio("P1",TradeList)
  P1$PortfolioName
  P1$Trades
  T3 = Trade("T3","Eq",200)
  P1$Trades[[length(P1$Trades)+1]]=T3
  
  
#Class - PortfolioSlice 
  #Data MarketData, portfolio
  #Member for Price Info, and Value
  #Member for diagnostics - ie greeks
  
  PortfolioSlice = function(MarketData,Portfolio,TimeIndex){
    rtrn = list()
    class(rtrn)="PortfolioSlice"
    rtrn$MarketDataSlice = MarketDataSlice(MarketData,TimeIndex)
    rtrn$Portfolio = Portfolio
    rtrn$TimeIndex = TimeIndex
    return(rtrn)
  }
  
  #PricingMethod
  #Polymorphic! Already defined for trade but am overloading here  
  Price.PortfolioSlice = function(PortSlice){ 
              PriceVec = numeric(length(PortSlice$Portfolio$Trades))
              PriceVec=sapply(PortSlice$Portfolio$Trades,Price,PortSlice$MarketDataSlice)
              names(PriceVec)=sapply(PortSlice$Portfolio$Trades, function(Trade){return(Trade$Name)})#Adds Names to cols
              return(PriceVec)
  }
  
  #ValueMethod
  Value.PortfolioSlice = function(PortSlice){
              Values = sapply(PortSlice$Portfolio$Trades, Value, PortSlice$MarketDataSlice)             
              names(Values)=sapply(PortSlice$Portfolio$Trades,function(Trade){return(Trade$Name)})#Adds Names to cols
              return(Values)}
  

  #Test
  P1Slice = PortfolioSlice(MarketData=MD1,Portfolio=P1,1)
  Price(P1Slice)
  Value(P1Slice)
