library(cv.ts)
require(reshape2)
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
result_ets <- cv.ts(fcastts, etsForecast, myControl,ic='bic')
#result_caret <- cv.ts(fcastts, caretForecast, myControl)
result_mean <- cv.ts(fcastts, meanForecast, myControl)
result_lm <- cv.ts(fcastts, lmForecast, myControl)
result_naive <- cv.ts(fcastts, snaiveForecast,myControl)
result_autoarima <- cv.ts(fcastts, auto.arimaForecast, myControl,ic='bic')
result_sts <- cv.ts(fcastts, stsForecast, myControl)
result_tbats <- cv.ts(fcastts, tbatsForecast, myControl)
result_nn <- cv.ts(fcastts, nnetarForecast, myControl)
result_rw <- cv.ts(fcastts, rwForecast, myControl)
result_theta <- cv.ts(fcastts, thetaForecast, myControl)


plotData <- data.frame(
  horizon=1:12,
  ets = result_ets$results$MAPE[1:12],
  mean = result_mean$results$MAPE[1:12],
  lm = result_lm$results$MAPE[1:12],
  naive = result_naive$results$MAPE[1:12],
  arima_aa = result_autoarima$results$MAPE[1:12],
  sts = result_sts$results$MAPE[1:12],
  tbats = result_tbats$results$MAPE[1:12],
  nn = result_nn$results$MAPE[1:12],
  rw = result_rw$results$MAPE[1:12],
  theta = result_theta$results$MAPE[1:12]
)


plotData <- melt(plotData, id.vars='horizon', value.name='MAPE', variable.name='model')
ggplot(plotData, aes(horizon, MAPE, color=model)) + geom_line() + 
  theme_bw() + 
  ggtitle("Forecast Cross Validation Performance Results")

#choose the best fit and forecast away
fit <- thetaf(fcastts)
plot(forecast(fit, h=12))
