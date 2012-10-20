#Trade Strategy classes
source("R/PortfolioClasses.R")
  #TradeStrategy
  #Data - MDobject, Portfolio, time, PL, PortfolioTS
  #Methods - initialise constructor, updatePort, stepStrat, runStrat 
 TradeStrategy = function(Portfolio,MarketData,InitialTimeIndex){
    rtrn = list()
    class(rtrn)="TradeStrategy"
    rtrn$MarketData = MarketData
    rtrn$Portfolio = Portfolio
    rtrn$CurrentTime = InitialTimeIndex
    rtrn$Results = data.frame()
    return(rtrn)
  }

  #Accessors
  #setMethod("getCT","TradeStrategy",function(object){return(object@CurrentTime)})
  #setMethod("getData","TradeStrategy",function(object){return(object@MarketData@Data)})
  #setMethod("getPortfolio","TradeStrategy",function(object){return(object@Portfolio)})

  #updatePortfolio implementation
  #This function defines trade strategy. 
  #It tells how to update notionals given MD and current portfolio
  #removeGeneric("updSig")
  updSig <- function(object,...) UseMethod("updSig") #You have to put this random line in when you first define polymorpic function
  updSig.TradeStrategy <- function(object){
              time = object$CurrentTime
              Data0 = PortfolioSlice(object$MarketData,object$Portfolio,(time-1))$MarketDataSlice$Data
              Data1 = PortfolioSlice(object$MarketData,object$Portfolio, time)$MarketDataSlice$Data
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
          


  
  #updatePortfolio implementation
  #This function defines trade strategy. It tells how to update notionals TradeStrategy MD
  
  updatePortfolio <- function(object,...) UseMethod("updatePortfolio") #You have to put this random line in when you first define polymorpic function
  updatePortfolio.TradeStrategy = function(object){
                  signal = updSig(object)
                  tradeNumber = length(object$Portfolio$Trades)+1
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
                    addPrtString=paste("object$Portfolio = addTrade(object$Portfolio,", tradeName,")",sep="")                  
                    cmdAdd = parse(text=addPrtString)
                    eval(cmdAdd) 
					
					#Update Cash for trade
					MDStmp = MarketDataSlice(MarketData = MD, TimeIndex = object$CurrentTime ) 
					Trades = object$Portfolio$Trades
					object$Portfolio$Trades[[1]]$Notional  = object$Portfolio$Trades[[1]]$Notional - Value(Trades[[length(Trades)]],MDStmp)
				
					
										
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
                    addPrtString=paste("object$Portfolio = addTrade(object$Portfolio,", tradeName,")",sep="")                  
                    cmdAdd = parse(text=addPrtString);eval(cmdAdd)
                    
					#Update Cash for trade
					MDStmp = MarketDataSlice(MarketData = MD, TimeIndex = object$CurrentTime ) 
					Trades = object$Portfolio$Trades
					object$Portfolio$Trades[[1]]$Notional  = object$Portfolio$Trades[[1]]$Notional - Value(Trades[[length(Trades)]],MDStmp)
					
					
					
                    } else {
                    #Do nothing
                  }
              
                  object$CurrentTime = object$CurrentTime + 1
				  #Update Cash for any trades
					
	
				  
				  return(object)
              }
            

  
runStrategy <- function(object,...) UseMethod("runStrategy") #You have to put this random line in when you first define polymorpic function			
#Below is function that loops over time updating Portfolio using the signal from the MA
runStrategy.TradeStrategy = function(object){
			timeInd = object$CurrentTime
			maxLoop = length(index(object$MarketData$Data))
			
			#object@Results = data.frame( Time = 0,  Value = 0, Signal = "hold") 
			object$Results = data.frame(matrix(nrow = (maxLoop-timeInd), ncol =3))
			colnames(object$Results)=c("Time","Value","Signal")

			for( i in 1:(maxLoop-timeInd))
			{
				signal = updSig(object)
				PS = PortfolioSlice(object$MarketData  ,object$Portfolio, timeInd)
				timeInd = object$CurrentTime
				time = index(object$MarketData$Data)[timeInd]
				object$Results[i,] = c(time,sum(Value(PS)),signal)
				object = updatePortfolio(object)
			}	
			object$Results$Time =as.numeric(object$Results$Time)
			return(object)
		}

