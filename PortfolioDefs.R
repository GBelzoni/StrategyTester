source("http://www.rmetrics.org/Rmetrics.R")
#install.Rmetrics()

library(xts)
library(zoo)
library(quantmod)
library(PerformanceAnalytics)
library(debug)

getSymbols('^AORD',src='yahoo',from="2007-01-01",to=Sys.Date())
getSymbols('^FTSE',src='yahoo',from="2007-01-01",to=Sys.Date())
getSymbols('^HSI',src='yahoo',from="2007-01-01",to=Sys.Date())
getSymbols('^DJI',src='yahoo',from="2007-01-01",to=Sys.Date())
getSymbols('^GSPC',src='yahoo',from="2007-01-01",to=Sys.Date())
getSymbols('KO',src='yahoo',from="2007-01-01",to=Sys.Date())
getSymbols('PEP',src='yahoo',from="2007-01-01",to=Sys.Date())


getFX('GBP/USD',src='yahoo',from=Sys.Date()-499,to=Sys.Date())

plot(GBPUSD['2011-06-01/'])
head(GBPUSD)
Dates=c( '2012-06-22','2012-05-24','2012-04-23',
         '2012-03-22','2012-02-28','2012-01-26',
         '2011-12-23','2011-11-25','2011-10-27',
         '2011-09-26','2011-08-26','2011-07-26')
1/GBPUSD[Dates] 

write.table(GBPUSD[Dates],"clipboard",row.names=F)

writeClipboard(coredata(GBPUSD[Dates])[,1])

GBPUSD[]

Spread=KO-PEP
chartSeries(KO)
chartSeries(PEP)

chartSeries(spreadGSPCDJI,theme="white")
head(AORDFTSE)
plot(AORD)
str(AORD)
names(AORD)
abline(lm(spreadGSPCDJI[,4] ~ index(spreadGSPCDJI)))

head(AORD$AORD.Close)

#Class - Market Data
  #Data member is an xts object have to format data into this to use
  setClass("MarketData",
      representation(
        Data = "xts"
        )
  )
  #Methods: GetData, UpdateData, currentData
  
  MD1 = new("MarketData",Data=Spread)
  length(MD1@Data)
  str(MD1@Data)
  #quantmod chart
  chartSeries(MD1@Data,theme="white",subset='2009-03-30::2012-06-10')

  
  EMA1=addEMA(n=10,col=2)
  EMA1Vals=EMA1@TA.values
  EMA2=addEMA(n=20)
  EMA2Vals=EMA2@TA.values
  EMA3=addEMA(n=100,col=3)
  head(EMA1Vals)
  addBBands()  
  
  Series0 = MD1@Data
  Series1 = as.xts(zoo(EMA1Vals,order.by=index(MD1@Data)))
  Series2 = as.xts(zoo(EMA2Vals,order.by=index(MD1@Data)))

  #Strategy - Crossover
  #assumes Series1 and Series 2 are xts
  S1GreaterThanS2 = sign(Series1-Series2)
  CrossOverSignal = lag(S1GreaterThanS2,1)*S1GreaterThanS2
  CrossOverPoints = which(CrossOverSignal==-1)
  CrossOverDates = index(S1GreaterThanS2)[CrossOverPoints]  

  addPoints(CrossOverDates,Series0[CrossOverDates,1],col=1)
  points(Series0[CrossOverPoints,1],col=2)
  chartSeries(Series0[CrossOverPoints])
  
  AORDMean=rollmean(MD1@Data,10)
  
  head(AORDMean)
  chartSeries(AORDMean,theme="white",subset='last 12 month')  
  addEMA(n=20,)  


#Exponentially Weighted moving average

EWMA_point = function( x, weight_=0.25){
  
  WeightedSum = x[1] 
  for(i in 2:length(x))
  {
    WeightedSum = weight_*x[i] + (1- weight_) * WeightedSum
  }
  
  return(WeightedSum)
  
}


EWMA = function(x,k=10,weight=0.25) #needs zoo::rollapply
{
  result = rollapply(x,k,EWMA_point, weight_=weight,align="right")
  
  return(as.xts(result))
  
}

xxx=FTSE['2011-03-30::2012-06-10']
yyy= EWMA(xxx,10,0.1)
zzz= EWMA(xxx, 50,0.1)
plot(xxx)
lines(yyy[,1],col=2)
lines(zzz[,1],col=4)
head(yyy[,1])
head(xxx[,1])

#Class - Market Data Analytics
    #Filters, Moving Averages, Correlations, PCA, spreads
    



#Class - Trade
  #Time, Size, Type(Option), Type Info, Name
  
#Class - Portfolio
  #Trades, position size

#Class - Portfolio Analytics
  #Pricing 
  #Greeks
  #PL

#Trading Strategy
  #Inputs: Market Data, Market Analytics, Portfolio, Portfolio Analytics
  #Methods: Backtester
