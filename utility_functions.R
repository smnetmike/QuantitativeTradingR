bestArimaFitAICBased <- function(vect, pmax, dmax, qmax)
{
  final<-list(order=NULL,aic=Inf, arima=NULL)
  for(p in 1:pmax)
    for(d in 0:dmax)
      for (q in 1:qmax)
      {
        if (p==0 & q==0)
        {
          next
        }
        currentArimaFit <- tryCatch( arima(vect, order=c(p,d,q)), error=function(err) FALSE,
                                     warning=function(err) FALSE )
        
        if (!is.logical(currentArimaFit))
        {
          current.aic <- AIC(currentArimaFit)
          #cat("(", p, ", ",d, ", ",q, "): AIC = ",  current.aic, "\n")
          if( current.aic < final$aic)
          {
            final$aic <- current.aic
            final$order <- c(p,d,q)
            final$arima <- currentArimaFit
          }
        }
      }
      
  final
}

rollingGarchForecast <- function(windowLen, forecastLen, vect)
{
  #Check that the vector is >= windowLen + forecastLen
  
  forecasts = vector(mode = "numeric", length = forecastLen)
  for (d in 0:(forecastLen-1))
  {
    # Get the rolling window
    vectPart = vect[(1+d):(windowLen+d)]
    
    # Get the best arima model
    final <- bestArimaFitAICBased(vectPart, 4, 1, 4)
    
    #Specify the garch model
    garchSpec <- ugarchspec(variance.model=list(garchOrder=c(1,1)), 
                            mean.model=list(armaOrder=c(final$order[1],final$order[3]), include.mean=TRUE),
                            distribution.model="sged")
    
    #Fitting the garch model
    garchFit = tryCatch(ugarchfit(garchSpec, vectPart, solver='hybrid', error= function(e) e,
                                  warning=function(w) w))
    
    #Go long if no convergence, or choose based on the forecast
    if (is(garchFit, "warning"))
    {
      forecasts[d+1] = paste(index(vect[(windowLen+d+1)]), 1, sep=",")
    } 
    else
    {
      nextForecast = ugarchforecast(garchFit, n.ahead=1)
      ind = nextForecast@forecast$seriesFor
      forecasts[d+1] = paste(colnames(ind),ifelse(ind[1]<0, -1, 1), sep=",")
    }
    
    # Saving the date
    print(forecasts[d+1])
  }
  
  #Concatenating forecasts in a string
  forecasts_all <- forecasts[1]
  for(c in 2:forecastLen)
  {
    forecasts_all <- paste(forecasts_all, forecasts[c], sep="\n")
  }
  
  #Transforming in a XTS instance
  spArimaGarch = as.xts(
    read.zoo(text = forecasts_all, format="%Y-%m-%d", header=F, sep=",")
  )
  
  spArimaGarch
}
