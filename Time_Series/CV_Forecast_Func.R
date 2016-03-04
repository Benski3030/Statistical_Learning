#' Mean forecast wrapper
#' @export
meanForecast <- function(x,h,...) {
  forecast::meanf(x, h, ..., level=99)$mean
}

#' Naive forecast wrapper
#' @export
naiveForecast <- function(x,h,...) {
  forecast::naive(x, h, ..., level=99)$mean
}

#' Seasonal naive forecast wrapper
#' @export
snaiveForecast <- function(x,h,...) {
  forecast::snaive(x, h, ..., level=99)$mean
}

#' Random walk forecast wrapper
#' @export
rwForecast <- function(x,h,...) {
  forecast::rwf(x, h, ..., level=99)$mean
}

#' Theta forecast wrapper
#' @export
thetaForecast <- function(x,h,...) {
  out <- forecast::thetaf(x, h, ..., level=99)$mean
  return(out)
}

#' Linear model forecast wrapper
#' @export
lmForecast <- function(x,h,xreg=NULL,newxreg=NULL,...) {
  x <- data.frame(x)
  colnames(x) <- 'x'
  if (is.null(xreg) & is.null(newxreg)) {
    fit <- tslm(x ~ trend + season, data=x, ...)
    return(forecast(fit, h=h, level=99)$mean)
  } else if ((!is.null(xreg)) & !(is.null(newxreg))) {
    newnames <- c('x',colnames(xreg))
    x <- cbind(x,xreg)
    colnames(x) <- newnames
    fmla <- as.formula(paste("x ~ trend + season +", paste(colnames(xreg), collapse= "+")))
    fit <- tslm(fmla, data=x, ...)
    return(forecast(fit, h=h, level=99, newdata=newxreg)$mean)
  } else {
    stop('xreg and newxreg must both be NULL or both be provided')
  }
}

#' Structural time series forecast wrapper
#' @export
stsForecast <- function(x,h,...) {
  fit <- StructTS(x, ...)
  forecast::forecast(fit, h=h, level=99)$mean
}

#' Stl forecast wrapper
#' @export
stl.Forecast <- function(x, h, method='ets', ...) {
  forecast::stlf(x, h=h, method, level=99, ...)$mean
}

#' Arima forecast wrapper
#' @export
arimaForecast <- function(x,h,xreg=NULL,newxreg=NULL,...) {
  fit <- forecast::Arima(x, xreg=xreg, ...)
  forecast::forecast(fit, h=h, level=99, xreg=newxreg)$mean
}

#' auto.arima forecast wrapper
#' @export
auto.arimaForecast <- function(x,h,xreg=NULL,newxreg=NULL,...) {
  fit <- forecast::auto.arima(x, xreg=xreg, ...)
  forecast::forecast(fit, h=h, level=99, xreg=newxreg)$mean
}

#' Ets forecast wrapper
#' @export
etsForecast <- function(x,h,...) {
  fit <- forecast::ets(x, ...)
  forecast::forecast(fit, h=h, level=99)$mean
}

#' BATS forecast wrapper
#' @export
batsForecast <- function(x,h,...) {
  fit <- forecast::bats(x, ...)
  forecast::forecast(fit, h=h, level=99)$mean
}

#' TBATS forecast wrapper
#' @export
tbatsForecast <- function(x,h,...) {
  fit <- forecast::tbats(x, ...)
  forecast::forecast(fit, h=h, level=99)$mean
}

#' NNetar forecast wrapper
#' @export
nnetarForecast <- function(x, h, nn_p=1, ...) {
  fit <- forecast::nnetar(x, p=nn_p, ...)
  forecast::forecast(fit, h=h, level=99)$mean
}

#' Caret forecast wrapper
#' @export
caretForecast <- function(x, h, xreg, newxreg=NULL,...) {
  myData <- data.frame(x=as.numeric(x), xreg)
  fit <- caret::train(x~., data=myData, ...)
  predict(fit, newdata=newxreg)
}

#' Create folds for a time series model UPDATE TO USE CARET!
#'
#' Define time-series cv indexes for caretForecast.
#'
#' @export
createTSfolds <- function(y, Min = max(2/3*length(y), 3), k = NA){
  out = plyr::llply(Min:(length(y) - 1), seq)
  if (!is.na(k)) {out = out[seq(1, length(out), k)]}
  names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), sep = "")
  return(out)
}

#Fix progress bar when using parallel backend?
#Add Garch, bats, tbats, fourrier, wavelets, and farima functions
#Fix RMSE calculation over all horizons
#Create tuning grids for forecast functions
#Add BoxCox.lambda method to tsControl

#best.ts: Return an object (for the best tune) with:
#	1. Data frame of parameters+error metric at selected horizon
#	2. Cross validated stats at each horizon+overall average for final model
#		2a. Additional row "best", where best model at each step (and horizon)
#			is used for the next
#	3. Data frame of best parameters at each point, selected by error metric at selected horizon
#	4. Best parameters for final model
#	5. Final model prediction data frame
#	6. Each step best model prediction data frame
#	7. Actuals data frame

#' Test if an object exists
#' @export
testObject <- function(object){
  exists(as.character(substitute(object)))
}

#' Default summary function
#' @export
tsSummary <- function(P,A) {
  data.frame((as.data.frame(accuracy(P,A))))
}

#' Default Cross-validation control
#' @export
tseriesControl <- function(stepSize=1, maxHorizon=1, minObs=12, fixedWindow=TRUE,
                           summaryFunc=tsSummary, preProcess=FALSE, ppMethod='guerrero'){
  list(stepSize=stepSize,
       maxHorizon=maxHorizon,
       minObs=minObs,
       fixedWindow=fixedWindow,
       summaryFunc=summaryFunc,
       preProcess=preProcess,
       ppMethod=ppMethod)
}

#' Function to cross-validate a time series.
#' @export
cv.ts <- function(x, FUN, tsControl=tseriesControl(), xreg=NULL, progress=TRUE, packages=NULL, ...) {
  
  #Load required packages
  stopifnot(is.ts(x))
  stopifnot(is.data.frame(xreg) | is.matrix(xreg) | is.null(xreg))
  
  #Load parameters from the tsControl list
  stepSize <- tsControl$stepSize
  maxHorizon <- tsControl$maxHorizon
  minObs <- tsControl$minObs
  fixedWindow <- tsControl$fixedWindow
  summaryFunc <- tsControl$summaryFunc
  preProcess <- tsControl$preProcess
  ppMethod <- tsControl$ppMethod
  
  #Make sure xreg object is long enough for last set of forecasts
  if (! is.null(xreg)) {
    xreg <- as.matrix(xreg)
    
    if (nrow(xreg)<length(x)+maxHorizon) {
      warning('xreg object too short to forecast beyond the length of the time series.
              Appending NA values to xreg')
      nRows <- (length(x)+maxHorizon)-nrow(xreg)
      nCols <- dim(xreg)[2]
      addRows <- matrix(rep(NA,nCols*nRows),nrow=nRows, ncol=nCols)
      colnames(addRows) <- colnames(xreg)
      xreg <- rbind(xreg,addRows)
    }
    
  }
  
  #Define additional parameters
  freq <- frequency(x)
  n <- length(x)
  st <- tsp(x)[1]+(minObs-2)/freq
  
  #Create a matrix of actual values.
  #X is the point in time, Y is the forecast horizon
  #http://stackoverflow.com/questions/8140577/creating-a-matrix-of-future-values-for-a-time-series
  formatActuals <- function(x,maxHorizon) {
    actuals <- outer(seq_along(x), seq_len(maxHorizon), FUN="+")
    actuals <- apply(actuals,2,function(a) x[a])
    actuals
  }
  
  actuals <- formatActuals(x,maxHorizon)
  actuals <- actuals[minObs:(length(x)-1),,drop=FALSE]
  
  #Create a list of training windows
  #Each entry of this list will be the same length, if fixed=TRUE
  steps <- seq(1,(n-minObs),by=stepSize)
  
  #Set progressbar
  combine <- rbind
  if (progress) {
    f <- function(){
      pb <- txtProgressBar(1,length(steps)-1,style=3)
      count <- 0
      function(...) {
        count <<- count + length(list(...)) - 1
        setTxtProgressBar(pb,count)
        Sys.sleep(0.01)
        flush.console()
        rbind(...)
      }
    }
    combine <- f()
  }
  
  #At each point in time, calculate 'maxHorizon' forecasts ahead
  forecasts <- foreach(i=steps, .combine=combine, .multicombine=FALSE,
                       .packages=c('forecast', 'caret', packages), .export=c('testObject', 'tsSummary', 'tseriesControl')) %dopar% {
                         
                         if (is.null(xreg)) {
                           if (fixedWindow) {
                             xshort <- window(x, start=st+(i-minObs+1)/freq, end=st+i/freq)
                             
                           } else {
                             xshort <- window(x, end=st + i/freq)
                           }
                           
                           if (preProcess) {
                             if (testObject(lambda)) {
                               stop("Don't specify a lambda parameter when preProcess==TRUE")
                             }
                             stepLambda <- BoxCox.lambda(xshort, method=ppMethod)
                             xshort <- BoxCox(xshort, stepLambda)
                           }
                           
                           out <- FUN(xshort, h=maxHorizon, ...)
                           
                           if (preProcess) {
                             out <- InvBoxCox(out, stepLambda)
                           }
                           
                           return(out)
                           
                         } else if (! is.null(xreg)) {
                           if (fixedWindow) {
                             xshort <- window(x, start=st+(i-minObs+1)/freq, end=st+i/freq)
                             xregshort <- xreg[((i):(i+minObs-1)),,drop=FALSE]
                           } else {
                             xshort <- window(x, end=st + i/freq)
                             xregshort <- xreg[(1:(i+minObs-1)),,drop=FALSE]
                           }
                           newxreg <- xreg[(i+minObs):(i+minObs-1+maxHorizon),,drop=FALSE]
                           
                           if (preProcess) {
                             if (testObject(lambda)) {
                               stop("Don't specify a lambda parameter when preProcess==TRUE")
                             }
                             stepLambda <- BoxCox.lambda(xshort, method=ppMethod)
                             xshort <- BoxCox(xshort, stepLambda)
                           }
                           
                           out <- FUN(xshort, h=maxHorizon,
                                      xreg=xregshort, newxreg=newxreg, ...)
                           
                           if (preProcess) {
                             out <- InvBoxCox(out, stepLambda)
                           }
                           
                           return(out)
                         }
                         
                       }
  
  #Extract the actuals we actually want to use
  actuals <- actuals[steps,,drop=FALSE]
  
  #Accuracy at each horizon
  out <- data.frame(
    plyr::ldply(1:maxHorizon,
                function(horizon) {
                  P <- forecasts[,horizon,drop=FALSE]
                  A <- na.omit(actuals[,horizon,drop=FALSE])
                  P <- P[1:length(A)]
                  P <- na.omit(P)
                  A <- A[1:length(P)]
                  summaryFunc(P,A)
                }
    )
  )
  
  #Add average accuracy, across all horizons
  overall <- colMeans(out)
  out <- rbind(out,overall)
  results <- data.frame(horizon=c(1:maxHorizon,'All'),out)
  
  #Add a column for which horizon and output
  return(list(actuals=actuals, forecasts=forecasts, results=results))
}



###########################
#Functions for testing MOVE TO EXAMPLES
###########################

if (FALSE){
  arimaForecast2 <- function(x,h,params,...) {
    require(forecast)
    order=c(params$p,params$d,params$q)
    Drift=params$Drift
    fit <- Arima(x, order=order, include.drift=Drift, ...)
    forecast(fit, h=h, level=99)$mean
  }
  
  best.ts <-  function(x, FUN, atHorizon, metric, tuneGrid, tsControl=tseriesControl(), ...) {
    out <- tuneGrid
    out[,metric] <- NA
    
    for (row in 1:nrow(tuneGrid)) {
      params <- tuneGrid[row,]
      tryCatch({
        result <- cv.ts(x, FUN, tsControl, params=params, ...)
        out[row,metric] <- result$results[atHorizon, metric]
      }, error = function(e) NA)
    }
    out
  }
  
  #model <- best.ts(a10, arimaForecast2,
  #                 atHorizon=1,
  #                 metric='MAPE',
  #                 tuneGrid=expand.grid(p=0:5, d=0:1, q=0:5, Drift=FALSE))
  #model
}
