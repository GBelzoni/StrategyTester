#Trade Strategy classes
source("PortfolioClasses.R")
  #TradeStrategy
  #Data - MDobject, Portfolio, time, PL, PortfolioTS
  #Methods - initialise constructor, updatePort, stepStrat, runStrat 
  setClass("TradeStrategy",
             representation(
               MarketData = "MarketData",
               Portfolio = "Portfolio",
               CurrentTime = "numeric", ##Hmm, have to make this adapt for different xts classes
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
      .Object@CurrentTime = 1 #(index(MarketData_@Data)[1])
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
  setMethod("getData","TradeStrategy",function(object){return(object@MarketData@Data)})
  setGenericVerif(x="getPortfolio",y  <- function(object){standardGeneric("getPortfolio")})
  setMethod("getPortfolio","TradeStrategy",function(object){return(object@Portfolio)})



  #updatePortfolio implementation
  #This function defines trade strategy. 
  #It tells how to update notionals given MD and current portfolio
  setGenericVerif(x="updSig",y  <- function(object){standardGeneric("updSig")})
  #removeGeneric("updSig")
  #THis is for MA crossover
  setMethod("updSig","TradeStrategy", 
           function(object){
              time = object@CurrentTime
              Data0 = MarketDataSlide(object@MarketData,(time-1))
              Data1 = MarketDataSlide(object@MarketData,(time))
              #Previous Moving average vals
              MAs0 = Data0@Data[,"MAs"]
              MAl0 = Data0@Data[,"MAl"]
              #Current MA vals
              MAs1 = Data1@Data[,"MAs"]
              MAl1 = Data1@Data[,"MAl"]
              
              #Check if there is an upcrossing this step
              if( ( MAs0 < MAl0 ) && ( MAs1 > MAl1 ) ){
                signal = "buy"
              } else if ( ( MAs0 > MAl0 ) && ( MAs1 < MAl1 )) {
                signal = "sell"
              } else {
                signal = "hold"
              }
            
              return(signal)
            }
          )


  
  #updatePortfolio implementation
  #This function defines trade strategy. It tells how to update notionals TradeStrategy MD
  
  setGenericVerif(x="updatePortfolio",y  <- function(object){standardGeneric("updatePortfolio")})
  #removeGeneric('updatePortfolio')
 # setMethod("updatePortfolio","TradeStrategy", 
         dbg =   function(object){
                  signal = updSig(object)
                  tradeNumber = length(getPortfolio(object)@Trades)+1
                  if(signal == "buy"){
                    #CreateBuyTrade
                    # This is a bit of a hack - have to create trade object
                    # with number as above. Job for pointers, but there are
                    # no pointers so create R command as string then eval string
                    tradeName = paste("Trade",tradeNumber,sep="")
                    trdDefString = paste(tradeName,"=Trade(\"",tradeName,"\",\"eq\",100)",sep="")
                    cmd = parse(text=trdDefString) #Convert string to R command
                    eval(cmd) #Evaluate command which should create new trade with correct number
                    
                  
                    #Add to portfolio
                    addPrtString=paste("object@Portfolio = addTrade(object@Portfolio,", tradeName,")",sep="")                  
                    cmdAdd = parse(text=addPrtString)
                    eval(cmdAdd) 
                    } else if(signal=="hold") {
                    #CreateSellTrade
                    # This is a bit of a hack - have to create trade object
                    # with number as above. Job for pointers, but there are
                    # no pointers so create R command as string then eval string
                    tradeName = paste("Trade",tradeNumber,sep="")
                    trdDefString = paste(tradeName,"=Trade(\"",tradeName,"\",\"eq\",-100)",sep="") #Note -100 for sell
                    cmd = parse(text=trdDefString) #Convert string to R command
                    eval(cmd) #Evaluate command which should create new trade with correct number
                    
                    
                    #Add to portfolio
                    addPrtString=paste("object@Portfolio = addTrade(object@Portfolio,", tradeName,")",sep="")                  
                    cmdAdd = parse(text=addPrtString);eval(cmdAdd)
                    
                    } else {
                    #Do nothing
                  }
              
                  object@CurrentTime = object@CurrentTime + 1
              }
            
            
      #      )

  #Testing
  
  TS1=TradeStrategy(MD,Port1)
  TS1=
  updSig(TS1,)  

  