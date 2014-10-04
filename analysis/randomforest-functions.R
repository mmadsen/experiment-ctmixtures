library(randomForest)
library(mmadsenr)
library(ROCR)


#' Calculates a random forest analysis of the specified data frame, with the specified class 
#' label field.  
#' 
#' @param data frame input data
#' @param column to use as class labels
#' @param vector fields to exclude from the random forest classifier.  should NOT include the class label field.
#' @param number fraction of the data frame to sample for out-of-sample test data
#' @param number number of trees to have the random forest classifier fit
#' @param number minimum node size for the decision trees
#' @param number number of variables to sample for each random tree
#' @return list List with fitted model, test classification rate, and confusion matrix for the test data
#' @export

do_binary_random_forest_roc <- function(df, class_field, fields_to_exclude, test_fraction = 0.1, numtrees = 1000, 
                             node_size = 1, variables_to_sample = 3) {
  # remove unwanted columns for this analysis
  df_dropped <- df[,!(names(df) %in% fields_to_exclude)]

  # split into random train and test data sets
  data <- random_split_dataset(df_dropped, test_fraction)

  form <- as.formula(paste(class_field, "~", ".", sep = " "))

  fit <- randomForest(form, data=data$train, ntree=numtrees, nodesize = node_size, mtry=variables_to_sample, test=data$test)
  
  rf.pr = predict(fit,type="prob",newdata=data$test)[,2]
  rf.pred = prediction(rf.pr, data$test[,class_field])
  rf.perf = performance(rf.pred, "tpr", "fpr")
  
  test_table <- table(data$test[,class_field], predict(fit, data$test[names(df_dropped)]))
  prediction <- sum(data$test[,class_field]==predict(fit, data$test[names(df_dropped)])) / nrow(data$test)
  error <- 1 - prediction
  
  ret <- list("fit"=fit, "prediction_rate" = prediction, "test_confusion" = test_table, 
              "test_error" = error, "roc" = rf.perf, "roc_pred" = rf.pred)
  ret
}

do_multiclass_random_forest <- function(df, class_field, fields_to_exclude, test_fraction = 0.1, numtrees = 1000, 
                                        node_size = 1, variables_to_sample = 3) {
  # remove unwanted columns for this analysis
  df_dropped <- df[,!(names(df) %in% fields_to_exclude)]
  
  # split into random train and test data sets
  data <- random_split_dataset(df_dropped, test_fraction)
  
  form <- as.formula(paste(class_field, "~", ".", sep = " "))
  
  fit <- randomForest(form, data=data$train, ntree=numtrees, nodesize = node_size, mtry=variables_to_sample, test=data$test)
  
  test_table <- table(data$test[,class_field], predict(fit, data$test[names(df_dropped)]))
  prediction <- sum(data$test[,class_field]==predict(fit, data$test[names(df_dropped)])) / nrow(data$test)
  error <- 1 - prediction
  
  ret <- list("fit"=fit, "prediction_rate" = prediction, "test_confusion" = test_table, 
              "test_error" = error)
  ret
}


randomforest_replicates <- function(df, class_field, fields_to_exclude, test_fraction = 0.2, numtrees = 500, 
                                       node_size = 1, variables_to_sample = 3, replicates = 100) {
  test_errors <- numeric(replicates)
  for( i in 1:replicates ) {
    m <- do_random_forest(df, class_field, fields_to_exclude, test_fraction, numtrees, node_size, variables_to_sample)
    test_errors[i] <- m$test_error
  }
  test_errors
}

plot_roc <- function(roc) {
  plot(roc,col=2,lwd=2)
  abline(a=0,b=1,lwd=2,lty=2,col="gray")
}

