#Need
# library(xts)
# library(zoo)
# library(quantmod)
# library(PerformanceAnalytics)
##This library stuffs up tab completes
#library(debug)

#Get All ordinaries data from yahoo and write to file - when uncommented
#getSymbols('^AORD',src='yahoo',from="2007-01-01",to=(Sys.Date()))
#write.table(AORD,"AORD.csv", sep=",",row.names=index(AORD))
#remove(AORD)

#Getting Data
AORD = as.xts(as.zoo(read.table("AORD.csv",header=T,sep=",")))
#Creating EMA using quantmod/Performance Analytics
chartData = (AORD['2008-02::2008-04'])
chartSeries(chartData,theme="white")
EMA1=addEMA(n=10,col=2)
EMA1Vals=EMA1@TA.values
EMA2=addEMA(n=20)
EMA2Vals=EMA2@TA.values

index(chartData)
Series1 = as.xts(zoo(EMA1Vals,order.by=index(chartData)));colnames(Series1)="MAs"
Series2 = as.xts(zoo(EMA2Vals,order.by=index(chartData)));colnames(Series2)="MAl"
chartData = cbind.xts(chartData,Series1,Series2 )
colnames(chartData)

source('TradeStrategyClasses.R')

#Running strat
  MD = MarketData(chartData) #Initialise MarketData
  MDS1 = MarketDataSlide(MarketData_=MD, TimeIndex_= 10)
  T1 = Trade("Cash","Cash",100) #Initialize trade type and notional
  T2 = Trade("Eq1","Eq",0) #Iniitalize eq trade type and notional
  Port1 = Portfolio(PortfolioName_="Portfolio1",Trades_=c(T1,T2))
  PortSlide1 = PortfolioSlide(Portfolio_=Port1, MarketDataSlide_=MDS1)
  Value(PortSlide1)    
  Price(PortSlide1)    
    
    
#Test MD
MD1 = MarketData(AORD)


#Test Trade Class
  T1 = Trade("Cash","Cash",100)
  T2 = Trade("Eq1","Eq",100)
  getNotional(T2)
  TradeList = c(T1,T2)  
  (Price(T2,MDSlide1))
  (Value(T2,MDSlide1))

#Test Portfolio Class
  P1=Portfolio("P1",TradeList)
  P1@PortfolioName
  P1@Trades

 #Test
  P1Slide = PortfolioSlide(P1,MDSlide1)
  Price(P1Slide)
  (Value(P1Slide)) 
#Trade Strat
 TS1=TradeStrategy(MD1,P1)
  TS1