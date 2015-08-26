library(glmnet)
library(useful)
library(foreach)
library(dplyr)
library(reshape2)
library(stringr)

set.seed(12345)
options(scipen = 10)

#Load the sampple data
# connect to the sqlite database
setwd("c:\foo")

#load data
tsdata <- read.csv("foo.csv")
tsdata <- na.omit(tsdata)

#Create a matrix of dependent variables: Elastic net can only operate against matrices
ebtsx <- build.x(y ~ . -1,data=tsdata, contrasts=FALSE)
class(ebtsx)
#Create a matrix for the independent variable
ebtsy <- build.y(y ~ . -1,data=tsdata)

#Perform cross validation to find the best alpha value 0 = ridge, 1 = lasso
#Create Folds and Alphas
theFolds <- sample(rep(x = 1:5, length.out = nrow(ebtsx)))
#Determine the sequence of alpha values ranging from 0 to 1
alphas <- seq(from = 0, to = 1, by = 0.05)
before <- Sys.time()
#Create a function to run a cv elastic net for the data fro each step in the alpha
acsDouble <- foreach(i=1:length(alphas), .errorhandling="pass",
                     .inorder=FALSE, .multicombine=TRUE,
                     .export=c("ebtsx", "ebtsy", "alphas", "theFolds"),
                     .packages="glmnet") %dopar%
                     {
                       print(alphas[i])
                       cv.glmnet(x=ebtsx, y=ebtsy, family="gaussian", nfolds=5,
                                 foldid=theFolds, alpha=alphas[i])
                     }
#Determine the system time
after <- Sys.time()
after - before

sapply(acsDouble, class)

#Extract all the lambda values and place them in a data frame from each model
extractGlmnetInfo <- function(object)
{
  # find lambdas
  lambdaMin <- object$lambda.min
  lambda1se <- object$lambda.1se
  
  # figure out where those lambdas fall in the path
  whichMin <- which(object$lambda == lambdaMin)
  which1se <- which(object$lambda == lambda1se)
  
  # build a one line data.frame with each of the selected lambdas and
  # its corresponding error figures
  data.frame(lambda.min=lambdaMin, error.min=object$cvm[whichMin],
             lambda.1se=lambda1se, error.1se=object$cvm[which1se])
}

# apply that function to each element of the list
# combine it all into a data.frame
alphaInfo <- Reduce(rbind, lapply(acsDouble, extractGlmnetInfo))

# could also be done with ldply from plyr
alphaInfo2 <- plyr::ldply(acsDouble, extractGlmnetInfo)
identical(alphaInfo, alphaInfo2)
alphaInfo$Alpha <- alphas

alphaMelt <- melt(alphaInfo, id.vars="Alpha", value.name="Value",
                  variable.name="Measure")
alphaMelt$Type <- str_extract(string=alphaMelt$Measure, pattern="(min)|(1se)")

# some housekeeping
alphaMelt$Measure <- str_replace(string=alphaMelt$Measure,
                                 pattern="\\.(min|1se)",replacement="")

alphaCast <- dcast(alphaMelt, Alpha + Type ~ Measure, value.var="Value")

ggplot(alphaCast, aes(x=Alpha, y=error)) +
  geom_line(aes(group=Type)) +
  facet_wrap(~Type, scales = "free_y", ncol=1) +
  geom_point(aes(size=lambda)) + 
  theme_bw()


#Cross validate the model
acsCV1 <- cv.glmnet(x = ebtsx, y = ebtsy, type.measure = "mse", family="gaussian",
                    alpha = alphaInfo$Alpha[which.min(alphaInfo$error.1se)])
acsCV1$lambda.1se
#View the created coefficients
plot(acsCV1)
coef(acsCV1, s = "lambda.1se")
plot(acsCV1$glmnet.fit, xvar = "lambda")
abline(v = log(acsCV1$lambda.min), lty = 1)
abline(v = log(acsCV1$lambda.1se), lty = 2)
plot(acsCV1$glmnet.fit, xvar = "dev")

#Create a coefficient matrix
theCoef <- as.matrix(coef(acsCV1, s = "lambda.1se"))
coefDF <- data.frame(Value = theCoef, Coefficient = rownames(theCoef))
coefDF <- coefDF[nonzeroCoef(coef(acsCV1, s = "lambda.1se")), ]
ggplot(coefDF, aes(x = X1, y = reorder(Coefficient, X1))) +
  geom_vline(xintercept = 0, color = "red", linetype = 2, size=1.25) +
  geom_point(color = "blue", size=5) + labs(x = "Value",
                                            y = "Coefficient", title = "Coefficient Plot") + 
  theme_bw()

predicted <- data.frame(enet = predict(acsCV1,type="response",newx=ebtsx))
actual <- tsdata$y

rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
rsq

RMSE <- sqrt(mean((actual-predicted)^2))
RMSE

tsdata$enet <- predicted$X1


p <- ggplot(tsdata, aes(x = enet, y = reorder(state, y))) +
  geom_point(size=3, color='red') +
  theme_classic() +
  ggtitle("Model Predictions(Black Dots) v. Actual (Red Dots)")
p + geom_point(aes(x = tsdata$y, y = reorder(state, tsdata$y)),size=3) +
  xlim(0,4300) + 
  xlab("Number Of Y") +
  ylab(NULL) + 
  xlim(0,2100) + 
  theme_bw() 


