
savepar <- par(mfrow=c(3,3), mar=c(4,4,2,0.5))

## This data is taken from sample code shipped with QuantLib 0.9.7
## from the file Examples/Swap/swapvaluation
params <- list(tradeDate=as.Date('2004-09-20'),
               settleDate=as.Date('2004-09-22'),
               dt=.25,
               interpWhat="discount",
               interpHow="loglinear")
setEvaluationDate(as.Date("2004-11-22"))

## We get numerical issue for the spline interpolation if we add
## any on of these three extra futures -- the original example
## creates different curves based on different deposit, fra, futures
## and swap data
tsQuotes <- list(d1w = 0.0382,
                 d1m = 0.0372,
                 d3m = 0.0363,
                 d6m = 0.0353,
                 d9m = 0.0348,
                 d1y = 0.0345,
#                 fut1=96.2875,
#                 fut2=96.7875,
#                 fut3=96.9875,
#                 fut4=96.6875,
#                 fut5=96.4875,
#                 fut6=96.3875,
#                 fut7=96.2875,
#                 fut8=96.0875,
                 s2y = 0.037125,
                 s3y = 0.0398,
                 s5y = 0.0443,
                 s10y = 0.05165,
                 s15y = 0.055175)

times <- seq(0,10,.1)

# Loglinear interpolation of discount factors
curves <- DiscountCurve(params, tsQuotes, times)
plot(curves,setpar=FALSE)

# Linear interpolation of discount factors
params$interpHow="linear"
curves <- DiscountCurve(params, tsQuotes, times)
plot(curves,setpar=FALSE)

# Spline interpolation of discount factors
params$interpHow="spline"
## NB Commented out for numerical issues under current parameterisation
curves <- DiscountCurve(params, tsQuotes, times)
plot(curves,setpar=FALSE)

par(savepar)