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

AORD = as.xts(as.zoo(read.table("../Data/AORD.csv",header=T,sep=",")))
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

#Load Classes up to StrategyTester
source('PortfolioClasses.R')


MD = MarketData(chartData) #Create Market Data

#Create trades and value against a slice of Market Data
T1 = Trade("Cash","Cash",100) #Create Trade Objects
T2 = Trade("AORD","Eq",100)
MDSlice = MarketDataSlice(MD,1)#Create MarketData time slice.Port1=Portfolio("Port1",c(T1,T2)) #Create Portfolio Objects
Value(T1,MDSlice)
Value(T2,MDSlice)

#Put trades in a Portfolio and Value for a given slice of time
#PortfolioSlice constructor
time = 1 #Pice time
PortfolioSlice = PortfolioSlice(Port1,MD,time) #Create Portfolio slice
Price(PortfolioSlice) #Query slice for Price
Value(PortfolioSlice) #Query slice for Value
#Can add more query methods when necessary - e.g. Delta's, risk reports, etc

#TradeStrategy Example
#Initialise empty cash portfolio - strategy will trade accordingly
#I assume zero interest rate by setting price of cash =1. 
#Can change by having P_cash grow by interest rate - set up an ir profile
Cash=c(Trade(Name_="Cash",Type_="Cash",0))
Port_MAtest = Portfolio(PortfolioName_ = "Port_MAtest",Trades_ = Cash)
#Initialise strategy
TS1=TradeStrategy(MD,Port_MAtest,21)
#Check vals exist
TS1@MarketData@Data[TS1@CurrentTime,]

#The Trade Strategy class has some useful members
#- updSignal, this generates trade signal "buy", "hold", "sell"
	updSig(TS1) #Should be sell
# -updatePortfolio, this updates portfolio by making trades if signal requires
	length(TS1@Portfolio@Trades) #only cash in Portfolio
	TS1@CurrentTime #Current time index
	TS1=updatePortfolio(TS1)
	length(TS1@Portfolio@Trades) #now two trades as signal was sell
	TS1@CurrentTime #Current time index
# -runStrategy, loops updatePortfolio over Market Data and collects results
#Initialise strategy back to time 21 and only cash in Port
TS1=TradeStrategy(MD,Port_MAtest,21)
TS1 = runStrategy(TS1)


TradeEvents = which(Re != "hold")
TradeDate = index(MD@Data)[TradeEvents]
TradeSigs[TradeEvents]

TS1@Results


TS1@Portfolio@Trades

plot(TS1@Results[-1,1:2], type ='l')
Res2 = zoo(TS1@Results$Value,order.by=as.Date(TS1@Results$Time))
plot(Res2[-1,])
abline(h=0)


