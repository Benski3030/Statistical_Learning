#Multiple Validation Model for forecasting 
#Benjamin Harder
#8/19/2015

#Load the required libraries
library(MAPA)
library(forecast)
library(TStools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

setwd("drive/foo")

#Read in the data set
data <- read.csv('data.csv')

#Set the start date for you time series, and what numeration unit it began in
fcastts <- ts(data$foo, frequency=24)
tsdisplay(fcastts)
seasplot(fcastts,outplot=1)
seasplot(fcastts,outplot=2)
seasplot(fcastts,outplot=3)
seasplot(fcastts,outplot=4)
kdemode(fcastts,outplot=TRUE)
decomp(fcastts,outplot=TRUE,type="pure.seasonal",decomposition="additive")


#Create Training and Test Sets for the Data
train <- window(fcastts,start=1,end=6)
plot(train,main="Forecast Model - Training", type='l')
test <- window(fcastts,start=6)
plot(test,main="Forecast Model Testing", type='l')


#Begin creating multiple forecasts
#Create an ARIMA Forecast from the training data
fit <- auto.arima(train)
arimamodel <- forecast(fit, level=95, h=24)
plot(forecast(arimamodel))
forecastmodels <- data.frame(arimamodel)

#Theta method from training data
thetafc<- theta(train,h=24,outplot=2, multiplicative=FALSE)
forecastmodels$theta <- thetafc$frc

#ETS Model from training data
etsfc <- ets(train,model="MMM",damped=TRUE)
etsforecast <- forecast(etsfc, h=24)
plot(forecast(etsfc, h=24))
forecastmodels$ets <- etsforecast$mean

#Multiple Aggregation Prediction Algorithm
mapafit <- mapaest(train,outplot=0)
mapacalc(train,mapafit,outplot=2)
mapafor(train,mapafit,ifh=12,fh=0)
out <- mapafor(train,mapafit,conf.lvl = 0.95,fh=24)
plotmapa(mapafit)
mapaforecast <- data.frame(t(out$PI))
Pointfc <- data.frame(out$outfor)
MAPAts <- ts(Pointfc,start=6, end = 7, freq=23)
forecastmodels$MAPA <- Pointfc$out.outfor

#Create a Naive Forecast
meanforecast <- meanf(train,h=24)
plot(forecast(meanforecast, h=24))
meanforecast$mean <- meanforecast$mean
forecastmodels$Mean <- meanforecast$mean

#Create a Random Walk forecast
randomwalk <- rwf(train,h=24)
plot(forecast(randomwalk, h=24))
randomwalk$RandomWalk <- randomwalk$mean
forecastmodels$randomwalk <- randomwalk$mean

#Create a Seasonally Naive forecast
seasonalnaive <- snaive(train,h=24)
plot(forecast(seasonalnaive, h=24))
seasonalnaive$SeasonalNaive <- seasonalnaive$mean
forecastmodels$seasonalnaive <- seasonalnaive$mean

#Perform Neural Network Forecast
nnfcast <- nnetar(train, repeats=20, lambda=NULL)
plot(forecast(nnfcast))
nnet <- data.frame(forecast(nnfcast,h=24))
forecastmodels$nnet <- nnet$Point.Forecast
nnetts <- ts(nnet$Point.Forecast,start=6, end = 7, freq=23)

#Perform Holt-Winters Forecast
hwforecast <- HoltWinters(train, seasonal = "multiplicative",,
                          optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
                          optim.control = list())
plot(forecast(hwforecast,h=24))
hwforecast <- data.frame(forecast(hwforecast,h=24))
forecastmodels$holtwin <- hwforecast$Point.Forecast
hwforecastts <- ts(hwforecast$Point.Forecast,start=6, end = 7, freq=23)

#clean up the data frame a bit
forecastmodels <- forecastmodels[,c(-2,-3)]
colnames(forecastmodels) <- c("ARIMA","THETA", "ETS", "MAPA", 
                              "Mean", "RandomWalk","SeasonalNaive","nnet","Holtwinters")
Time <- c(rep(1:24))
forecastmodels$Time <- Time

#Compare the accuracy of all the models
d1 <- data.frame(accuracy(forecastmodels$ARIMA, test))
d2 <- data.frame(accuracy(forecastmodels$THETA, test))
d3 <- data.frame(accuracy(forecastmodels$ETS, test))
d4 <- data.frame(accuracy(forecastmodels$MAPA, test))
d5 <- data.frame(accuracy(forecastmodels$Mean, test))
d6 <- data.frame(accuracy(forecastmodels$RandomWalk, test))
d7 <- data.frame(accuracy(forecastmodels$SeasonalNaive, test))
d8 <- data.frame(accuracy(forecastmodels$nnet, test))
d9 <- data.frame(accuracy(forecastmodels$Holtwinters, test))

row.names(d1) <- "ARIMA"
row.names(d2) <- "THETA"
row.names(d3) <- "ETS"
row.names(d4) <- "MAPA"
row.names(d5) <- "Mean"
row.names(d6) <- "RandomWalk"
row.names(d7) <- "SeasonalNaive"
row.names(d8) <- "NNet"
row.names(d9) <- "Holtwinters"

master <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
View(master)

write.csv(forecastmodels,"forecastcollection.csv")

#rearrange the data for ggplot2
plotframe <- gather(forecastmodels, "Model", "Forecast", 1:9)

#plot the forecasts
ggplot(plotframe, aes(x=Time, y=Forecast, color=Model)) +
  geom_line(size = 1) +
  theme_classic()


#Choose the best and make the model
#Create an ARIMA Forecast from the full data
fit <- auto.arima(fcastts)
arimamodel <- forecast(fit, level=95, h=24)
finalforecast <- data.frame(arimamodel)

#Create a Seasonally Naive forecast
seasonalnaive <- snaive(fcastts,h=24)
seasonalnaive$SeasonalNaive <- seasonalnaive$mean
finalforecast$seasonalnaive <- seasonalnaive$mean

#ETS Model from training data
etsfc <- ets(fcastts,model="MMM",damped=TRUE)
etsforecast <- forecast(fcastts, h=24)
finalforecast$ets <- etsforecast$mean


write.csv(finalforecast, 
          "forecast.csv",
          row.names=F)


