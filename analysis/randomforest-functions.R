library(randomForest)


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

do_random_forest <- function(df, class_field, fields_to_exclude, test_fraction = 0.2, numtrees = 1000, 
                             node_size = 1, variables_to_sample = 3) {
  # remove unwanted columns for this analysis
  df_dropped <- df[,!(names(df) %in% fields_to_exclude)]
  
  # shuffled the data frame row-wise so that we can sample test and train data
  shuffled_df <- df_dropped[sample(nrow(df_dropped)),]
  indexes = sample(1:nrow(shuffled_df), size=test_fraction*nrow(shuffled_df))
  test = shuffled_df[indexes,]
  train = shuffled_df[-indexes,]
  
  form <- as.formula(paste(class_field, "~", ".", sep = " "))

  fit <- randomForest(form, data=train, ntree=numtrees, nodesize = node_size, mtry=variables_to_sample)
  
  test_table <- table(test[[class_field]], predict(fit, test[names(shuffled_df)]))
  error <- sum(test[[class_field]]==predict(fit, test[names(shuffled_df)])) / nrow(test)
  
  ret <- list("fit"=fit, "prediction_rate" = error, "test_confusion" = test_table)
  ret
}


