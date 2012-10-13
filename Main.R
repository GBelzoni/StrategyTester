#Need
rm(list=ls())
library(xts)
library(zoo)
library(quantmod)
library(PerformanceAnalytics)
##This library stuffs up tab completes
#library(debug)

#Get All ordinaries data from yahoo and write to file - when uncommented
#getSymbols('^AORD',src='yahoo',from="2007-01-01",to=(Sys.Date()))
#write.table(AORD,"AORD.csv", sep=",",row.names=index(AORD))
#remove(AORD)

#Getting Data

AORD = as.xts(as.zoo(read.table("AORD.csv",header=T,sep=",")))
#Creating EMA using quantmod/Performance Analytics
chartData = (AORD)#['2008-02::2009-02'])
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

#Load Classes up to StrategyTester
source('PortfolioClasses.R')


MD = MarketData(chartData) #Create Market Data
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
#Initialise empty cash portfolio
Cash=c(Trade(Name_="Cash",Type_="Cash",0))
Port_MAtest = Portfolio(PortfolioName_ = "Port_MAtest",Trades_ = Cash)
#Initialise strategy
TS1=TradeStrategy(MD,Port_MAtest)
#Have to move time forward till both MAs have values
TS1@CurrentTime= TS1@CurrentTime +28
#Check vals exist
TS1@MarketData@Data[TS1@CurrentTime,]


#Testing updSignal function
colnames(TS1@MarketData@Data)
updSig(TS1)

TS1@CurrentTime= 29
maxLoop = length(index(MD@Data)) -29
TradeSigs = character()

	#Get Dates where trades occur
	for(t in 1:maxLoop)
	{
		TradeSigs = c(TradeSigs,updSig(TS1))
		TS1@CurrentTime = TS1@CurrentTime +1
	}
	TradeEvents = which(TradeSigs != "hold")
	TradeDate = index(MD@Data)[TradeEvents]
	TradeSigs[TradeEvents]

#Testing Portfolio Update
#Pick Event time
TS1@CurrentTime=15+28
updSig(TS1) #Should be buy


#Run Strategy Loop
TS1 = init_port()
Dates = index(MD@Data)
timeInd = TS1@CurrentTime
maxLoop = length(index(MD@Data))
TS1@Results = data.frame( Time = 0,  Value = 0) 

for( i in 1:(maxLoop-timeInd))
{
	MDS = MarketDataSlide(MD, timeInd)
	updSig(TS1)
	TS1 = updatePortfoliodbg(TS1)
	PS = PortfolioSlide( getPortfolio(TS1), MDS)
	TS1@Results = rbind(TS1@Results , c(as.Date(Dates[timeInd]),sum(Value(PS))))
	timeInd = TS1@CurrentTime
	
}
TS1@Portfolio@Trades
TS1@Results
plot(TS1@Results[-1,], type ='l')
Res2 = zoo(TS1@Results$Value,order.by=as.Date(TS1@Results$Time))
plot(Res2[-1,])
abline(h=0)

a=1:10
tail(a,1)

init_port = function(){
	#TradeStrategy Example
	#Initialise empty cash portfolio
	Cash=c(Trade(Name_="Cash",Type_="Cash",0))
	Port_MAtest = Portfolio(PortfolioName_ = "Port_MAtest",Trades_ = Cash)
	#Initialise strategy
	TS1=TradeStrategy(MD,Port_MAtest)
	#Have to move time forward till both MAs have values
	TS1@CurrentTime= TS1@CurrentTime +42
	#Check vals exist
	TS1@MarketData@Data[TS1@CurrentTime,]
	return(TS1)
}