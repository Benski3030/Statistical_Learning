model_diagnostics <- function (predicted, actual, model_name = "Unkown Classifier" ) { #function and arguments
  my_tbl <- table(Actual <-  actual, Predicted <-   predicted) #compile inputs into table
  n <- sum(my_tbl) #total number of observations
  nc <- nrow(my_tbl) #total number of classes
  diag <- diag(my_tbl) #number of correctly classified observations per class 
  rowsums <- apply(my_tbl, 1, sum) # number of obeservations per class
  colsums <- apply(my_tbl, 2, sum) # number of predictions per class
  p <- rowsums / n #instances over the actual classes
  q <- colsums / n #instances over the predicted classes
  print(paste("Model Diagnostics:", model_name)) #prints out the model name
  accuracy <-  sum(diag) / n  #Accuracy measurement
  print(paste("The overall accuracy of the predictions is:", accuracy * 100,"%")) #pretty print accuracy
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
  print(paste("The average accuracy of the predictions is:", avgAccuracy * 100,"%")) 
  f1 <-  2 * precision * recall / (precision + recall) 
  print("The balance of Precision v. Recall is:")
  print(data.frame(precision, recall, f1)) #summary stats
  print("Precision is defined as: Out of the items that the classifier predicted to be correct, how many are truly correct?")
  print("Recall is defined as: Out of all the items that are truly correct, how many were found by the classifier?")
  expAccuracy <-  sum(p*q) #expected accuracy
  kappa <-  (accuracy - expAccuracy) / (1 - expAccuracy) #kappa statistic
  print(paste("The kappa statistic is", kappa, ", Which is the measure of agreement between the predictions and actual lables, 0 is bad, 1 is good"))
  if (accuracy > 0.8 & avgAccuracy > 0.8 & kappa > 0.8) { #quick guy check
    print("This appears to be a good model")
  } else {
    print("Something isn't right, check the model again")
  }
}
