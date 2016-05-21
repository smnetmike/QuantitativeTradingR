setwd("C:\\Users\\Netmike\\Documents\\R")
rm(list=ls())
source("utility_functions.R")

require(quantmod)
symb <- "AMZN"
getSymbols(symb, from="2013-01-01")
adClo_prices <- Ad(AMZN)
symb_rt <- diff(log(adClo_prices))

#Get rid of the NA component
symb_rt <- symb_rt[-1]

# create the forecasts vector to store prediction
require(rugarch)
require(timeSeries)
require(lattice)
WindowLength = 800
foreLength = length(symb_rt) - WindowLength
spArimaGarch <- rollingGarchForecast(WindowLength, 5, symb_rt)

spIntersect <- merge(spArimaGarch, symb_rt, all=F)
spArimaGarchReturns = spIntersect[,1] * spIntersect[,2]

# Create cumulative returns
spArimaGarchCurve = log(cumprod(1 + spArimaGarchReturns))
spBuyHoldAndBuy <- log(cumprod(1 + spIntersect[,2]))
spCombinedCurve <- log()






# Changing character vector to XTS type
spArimaGarch <- vector(mode = "numeric")
rnames <- vector(mode = "character")
forecasts_split <- strsplit(forecasts, ",")
for(f in 1:length(forecasts))
{
  spArimaGarch[f] <- as.integer(forecasts_split[[f]][2])
  rnames[f] <- forecasts_split[[f]][1]
}
dim(spArimaGarch) <- length(forecasts)
rownames(spArimaGarch) <- rnames
spArimaGarch <- as.xts(spArimaGarch)
write.csv(forecasts, file = "forecast_rolling.csv", row.names = FALSE)
spArimaGarch <- read.zoo(file = "forecast_rolling.csv", header=F, format = "%Y-%m-%d", sep = ",")
