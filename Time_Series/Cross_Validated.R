library(cv.ts)
require(reshape2)
require(ggplot2)
library(forecast)
library(foreach)
library(tseries)
set.seed(1231564)
theme_set(theme_minimal())

#Generate data for the forecasting
fcastts <- ts(data(AirPassengers))
fcastts <- AirPassengers

plot(AirPassengers, type = 'l')

diff(AirPassengers)
adf.test(diff(AirPassengers), alternative="stationary", k=0)

seasonplot(fcastts,col=rainbow(12),year.labels=TRUE)
plot(decompose(fcastts))
monthplot(fcastts)

#Define cross validation parameters
myControl <- tseriesControl(
  minObs=100,
  stepSize=1, 
  maxHorizon=12, 
  fixedWindow=TRUE,
  preProcess=TRUE,
  ppMethod='guerrero',
  summaryFunc=tsSummary
)

#Forecast using several models
result_naive <- cv.ts(fcastts, snaiveForecast,myControl)
result_autoarima <- cv.ts(fcastts, auto.arimaForecast, myControl,ic='bic')
result_ets <- cv.ts(fcastts, etsForecast, myControl,ic='bic')
result_theta <- cv.ts(fcastts, thetaForecast, myControl)
result_lm <- cv.ts(fcastts, lmForecast, myControl)
result_tbats <- cv.ts(fcastts, tbatsForecast, myControl)

#Plot error
plotData <- data.frame(
  horizon=1:12
  ,naive =result_naive$results$MAPE[1:12]
  ,arima=result_autoarima$results$MAPE[1:12]
  ,ets=result_ets$results$MAPE[1:12]
  ,theta=result_theta$results$MAPE[1:12]
  ,theta=result_lm$results$MAPE[1:12]
  ,tbats=result_tbats$results$MAPE[1:12]
)
plotData <- melt(plotData, id.vars='horizon', value.name='MAPE', variable.name='model')
ggplot(plotData, aes(horizon, MAPE, color=model)) + geom_line() + 
  theme_bw()

#choose the best fit and forecast away
fit <- tbats(fcastts)
plot(forecast(fit, h=12))
