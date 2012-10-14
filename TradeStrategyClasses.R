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
    definition= function(.Object,MarketData_,Portfolio_, InitialTimeIndex){
      .Object@MarketData = MarketData_
      .Object@Portfolio = Portfolio_
      .Object@CurrentTime = InitialTimeIndex #(index(MarketData_@Data)[1])
      Results = data.frame()
      return(.Object)
      }
    
    )
  
  #need to define generic method before we can define class method
  setGenericVerif(x="TradeStrategy",y  <- function(object){standardGeneric("TradeStrategy")})
  TradeStrategy  <- function(MarketData_,Portfolio_,InitialTimeIndex_=1){
    new(Class="TradeStrategy",MarketData=MarketData_,Portfolio=Portfolio_,InitialTimeIndex = InitialTimeIndex_)
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
	PS1 = PortfolioSlice(P1,MD1,1)@MarketDataSlice@Data
	setMethod("updSig","TradeStrategy", 
           function(object){
              time = object@CurrentTime
             
			  
			  Data0 = PortfolioSlice( object@Portfolio, object@MarketData,(time-1))@MarketDataSlice@Data
              Data1 = PortfolioSlice(object@Portfolio,object@MarketData, time)@MarketDataSlice@Data
              #Previous Moving average vals
              MAs0 = Data0[,"MAs"]
              MAl0 = Data0[,"MAl"]
              #Current MA vals
              MAs1 = Data1[,"MAs"]
              MAl1 = Data1[,"MAl"]
              
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
  setMethod("updatePortfolio","TradeStrategy", 
		  function(object){
                  signal = updSig(object)
                  tradeNumber = length(getPortfolio(object)@Trades)+1
                  if(signal == "buy"){
                    #CreateBuyTrade
                    # This is a bit of a hack - have to create trade object
                    # with number as above. Job for pointers, but there are
                    # no pointers so create R command as string then eval string
                    tradeName = paste("Trade",tradeNumber,sep="")
					#trdDefString = "<<TradeName>> = Trade(\"<<TradeName>>\",\"Eq\",<<Notional>>)"
					#(gsub("<<Notional>>", 100 , gsub("<<TradeName>>","blah",trdDefString)))
					trdDefString = paste(tradeName,"=Trade(\"",tradeName,"\",\"Eq\",100)",sep="")
					                    
					cmd = parse(text=trdDefString) #Convert string to R command
                    eval(cmd) #Evaluate command which should create new trade with correct number
                    
                  
                    #Add to portfolio
                    addPrtString=paste("object@Portfolio = addTrade(object@Portfolio,", tradeName,")",sep="")                  
                    cmdAdd = parse(text=addPrtString)
                    eval(cmdAdd) 
					
					#Update Cash for trade
					MDStmp = MarketDataSlide(MarketData_ = MD, TimeIndex_ = object@CurrentTime ) 
					Trades = getPortfolio(object)@Trades
					object@Portfolio@Trades[[1]]@Notional  = object@Portfolio@Trades[[1]]@Notional - Value(Trades[[length(Trades)]],MDStmp)
				
					
										
                    } else if(signal=="sell") {
                    #CreateSellTrade
                    # This is a bit of a hack - have to create trade object
                    # with number as above. Job for pointers, but there are
                    # no pointers so create R command as string then eval string
                    tradeName = paste("Trade",tradeNumber,sep="")
                    trdDefString = paste(tradeName,"=Trade(\"",tradeName,"\",\"Eq\",-100)",sep="") #Note -100 for sell
                    cmd = parse(text=trdDefString) #Convert string to R command
                    eval(cmd) #Evaluate command which should create new trade with correct number
                    
                    
                    #Add to portfolio
                    addPrtString=paste("object@Portfolio = addTrade(object@Portfolio,", tradeName,")",sep="")                  
                    cmdAdd = parse(text=addPrtString);eval(cmdAdd)
                    
					#Update Cash for trade
					MDStmp = MarketDataSlide(MarketData_ = MD, TimeIndex_ = object@CurrentTime ) 
					Trades = getPortfolio(object)@Trades
					object@Portfolio@Trades[[1]]@Notional  = object@Portfolio@Trades[[1]]@Notional - Value(Trades[[length(Trades)]],MDStmp)
					
					
					
                    } else {
                    #Do nothing
                  }
              
                  object@CurrentTime = object@CurrentTime + 1
				  #Update Cash for any trades
					
	
				  
				  return(object)
              }
            )

  
			
setGenericVerif(x="runStrategy",y  <- function(object){standardGeneric("runStrategy")})
#removeGeneric('updatePortfolio')
removeGeneric('runStrategy')
#setMethod("runStrategy","TradeStrategy", 
		#Below is function that loops over time updating Portfolio using the signal from the MA
runStrategy =		function(object){
			timeInd = object@CurrentTime
			maxLoop = length(index(MD@Data))
			
			#object@Results = data.frame( Time = 0,  Value = 0, Signal = "hold") 
			object@Results = data.frame(matrix(nrow = (maxLoop-timeInd), ncol =3))
			colnames(object@Results)=c("Time","Value","Signal")
			browser()
			for( i in 1:(maxLoop-timeInd))
			{
				signal = updSig(object)
				object = updatePortfolio(object)
				PS = PortfolioSlice( getPortfolio(object),  MD, timeInd)
				object@Results[i,] = c(object@Results , timeInd,sum(Value(PS)),signal)
				timeInd = object@CurrentTime
				
			}		
			
			return(object)
		}
#)
