model_diagnostics <- function (predicted, actual, model_name = "Unknown Classifier" ) { #function and arguments
  my_tbl <- table(Actual =  actual, Predicted = predicted) #compile inputs into table
  n <- sum(my_tbl) #total number of observations
  nc <- nrow(my_tbl) #total number of classes
  diag <- diag(my_tbl) #number of correctly classified observations per class 
  rowsums <- apply(my_tbl, 1, sum) # number of obeservations per class
  colsums <- apply(my_tbl, 2, sum) # number of predictions per class
  p <- rowsums / n #instances over the actual classes
  q <- colsums / n #instances over the predicted classes
  cat("Model Diagnostics:", model_name, "\n") #prints out the model name
  print(my_tbl)
  accuracy <-  sum(diag) / n  #Accuracy measurement
  cat("The overall accuracy of the predictions is:", accuracy * 100,"%", "\n") #pretty print accuracy
  precision <-  diag / colsums #calculate precision
  recall <-  diag / rowsums  #calculate recall
  oneVsAll <-  lapply(1 : nc, #one v. all approximation
                      function(i){
                        v <-  c(my_tbl[i,i],
                                rowsums[i] - my_tbl[i,i],
                                colsums[i] - my_tbl[i,i],
                                n-rowsums[i] - colsums[i] + my_tbl[i,i]);
                        return(matrix(v, nrow =  2, byrow =  T))})
  s <-  matrix(0, nrow <-  2, ncol <-  2)
  for(i in 1 : nc){s <-  s + oneVsAll[[i]]} #for loop for list index
  avgAccuracy <-  sum(diag(s)) / sum(s) #calucate the average accuracy
  cat("The average accuracy of the predictions is:", avgAccuracy * 100,"%", "\n") 
  f1 <-  2 * precision * recall / (precision + recall) 
  cat("\n","The balance of Precision v. Recall is:","\n")
  print(data.frame(precision, recall, f1)) #summary stats
  cat("\n","Precision is defined as: Out of the items that the classifier predicted to be correct, how many are truly correct?")
  cat("\n","Recall is defined as: Out of all the items that are truly correct, how many were found by the classifier?","\n")
  expAccuracy <-  sum(p*q) #expected accuracy
  kappa <-  (accuracy - expAccuracy) / (1 - expAccuracy) #kappa statistic
  cat("\n","The kappa statistic is", kappa, ", Which is the measure of agreement between the predictions and actual labes, 0 is bad, 1 is good")
  if (accuracy > 0.8 & avgAccuracy > 0.8 & kappa > 0.8) { #quick guy check
    cat("\n","This appears to be a good model","\n")
  } else {
    cat("\n","Something isn't right, check the model again")
  }
  if("pROC" %in% rownames(installed.packages()) == FALSE) {
    install.packages("pROC")
  }
  prednum <- as.numeric(predicted)
  actualnum <- as.numeric(actual)
  auc <- pROC::roc(actualnum, prednum)
  print (auc)
  plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
  abline(h=1,col='blue',lwd=2)
  abline(h=0,col='red',lwd=2)
}
