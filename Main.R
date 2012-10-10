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

AORD = as.xts(as.zoo(read.table("AORD.csv",header=T,sep=",")))
source('TradeStrategyClasses.R')
