model_diagnostics <- function (predicted, actual, model_name = "Unkown Classifier" ) {
  my_tbl <- table(Actual <-  actual, Predicted <-   predicted)
  n <- sum(my_tbl) # number of instances
  nc <- nrow(my_tbl) # number of classes
  diag <- diag(my_tbl) # number of correctly classified instances per class 
  rowsums <- apply(my_tbl, 1, sum) # number of instances per class
  colsums <- apply(my_tbl, 2, sum) # number of predictions per class
  p <- rowsums / n # distribution of instances over the actual classes
  q <- colsums / n # distribution of instances over the predicted classes
  print(paste("Model Diagnostics:", model_name))
  accuracy <-  sum(diag) / n 
  accuracy
  print(paste("The overall accuracy of the predictions is:", accuracy * 100,"%"))
  precision <-  diag / colsums 
  recall <-  diag / rowsums 
  oneVsAll <-  lapply(1 : nc,
                    function(i){
                      v <-  c(my_tbl[i,i],
                            rowsums[i] - my_tbl[i,i],
                            colsums[i] - my_tbl[i,i],
                            n-rowsums[i] - colsums[i] + my_tbl[i,i]);
                      return(matrix(v, nrow =  2, byrow =  T))})
  oneVsAll
  s <-  matrix(0, nrow <-  2, ncol <-  2)
  for(i in 1 : nc){s <-  s + oneVsAll[[i]]}
  avgAccuracy <-  sum(diag(s)) / sum(s)
  print(paste("The average accuracy of the predictions is:", avgAccuracy * 100,"%"))
  f1 <-  2 * precision * recall / (precision + recall) 
  print("The balance of Precision v. Recall is:")
  print("Precision is defined as: Out of the items that the classifier predicted to be correct, how many are truly correct?")
  print("Recall is defined as: Out of all the items that are truly correct, how many were found by the classifier?")
  print(data.frame(precision, recall, f1))
  expAccuracy <-  sum(p*q)
  kappa <-  (accuracy - expAccuracy) / (1 - expAccuracy)
  print(paste("The kappa statistic is", kappa, ", Which is the measure of agreement between the predictions and actual lables, 0 is bad, 1 is good"))
  if (accuracy > 0.8 & avgAccuracy > 0.8 & kappa > 0.8) {
    print("This appears to be a good model")
  } else {
    print("Something isn't right, check the model again")
  }
}
