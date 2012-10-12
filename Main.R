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


MD = MarketData(AORD) #Create Market Data
T1 = Trade("Cash","Cash",100) #Create Trade Objects
T2 = Trade("AORD","Eq",100)
Port1=Portfolio("Port1",c(T1,T2)) #Create Portfolio Objects

#Create timeslice info for different times
#TODO - rewrite classes to get rid of MarketDataSlice. Can include functionality in
#PortfolioSlice constructor
time = 1 #Pice time
MDSlice = MarketDataSlide(MD,1)#Create MarketData time slice.
PortfolioSlice = PortfolioSlide(Port1,MDSlice) #Create Portfolio slice
Price(PortfolioSlice) #Query slice for Price
Value(PortfolioSlice) #Query slice for Value
#Can add more query methods when necessary - e.g. Delta's, risk reports, etc

#TradeStrategy Example
TS1=TradeStrategy(MD1,P1)
TS1

