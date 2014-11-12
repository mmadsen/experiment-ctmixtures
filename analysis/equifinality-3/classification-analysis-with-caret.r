library(randomForest)
library(caret)
library(doMC)
library(mmadsenr)
library(futile.logger)

# Train and tune random forest classifiers for each of the three data sets coming out of the experiment
# "equifinality-3", for binary analysis. 
#
# Assumes that data-preparation.r has previously loaded CSV files and created binary 
# data files, stored in a local data directory.  
#
# Reduces the four class data into a two class problem for basic classifier analysis, and it splits off
# a test data set, placing it in the environment.  
# 
# NOTE:  This analysis takes a LONG time to run, so it is kept separate from the Rmarkdown
# analysis script.  At the completion of the analysis, it saves an environment image, which
# other scripts, such as RMarkdown documents, can load, and access the fitted model results.  
#
# Only the model training and fitting is done in this script.  The test data set is not analyzed
# here, since it can be done quickly enough that I want to be working in RMarkdown to examine different
# options.  

# Set up logging
log_file <- "classification.log"
flog.appender(appender.file(log_file), name='cl')

flog.info("Beginning classification analysis of equifinality-3 data sets", name='cl')

# set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
num_cores <- get_parallel_cores_given_os()
flog.info("Number of cores used in analysis: %s", num_cores, name='cl')
registerDoMC(cores = num_cores)

# load data frame, results in object "eq3_pop_df" in the workspace
# for dev, source the file "dev-Rprofile" first to set up a base data directory (yours may vary!)
#
pop_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-population-data.rda")
sampled_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-sampled-data.rda")
ta_sampled_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-ta-sampled-data.rda")


load(pop_data_file)
load(sampled_data_file)
load(ta_sampled_data_file)

flog.info("Loaded data file: %s", pop_data_file, name='cl')
flog.info("Loaded data file: %s", sampled_data_file, name='cl')
flog.info("Loaded data file: %s", ta_sampled_data_file, name='cl')

# make this repeatable - comment this out or change it to get a fresh analysis result
seed_value <- 23581321
set.seed(seed_value)
flog.info("RNG seed to replicate this analysis: %s", seed_value, name='cl')


# tuning grid of parameters and tuning cross-validation parameters
mtry_seq <- seq(from=2, to=12, by=2)
flog.info("Tuning random forest parameter mtry using vals: %s", mtry_seq, name='cl')
fit_grid <- expand.grid(mtry=mtry_seq)

cv_num <- 10
cv_repeats <- 10

flog.info("Tuning performed by repeated CV, %s folds with %s repeats", cv_num, cv_repeats, name='cl')

fit_control <- trainControl(method="repeatedcv", 
                            number=cv_num, 
                            repeats=cv_repeats, 
                            allowParallel = TRUE,
                            ## Estimate class probabilities
                            classProbs = TRUE,
                            ## Evaluate performance using 
                            ## the following function
                            summaryFunction = twoClassSummary)


# Set up sampling of train and test data sets
training_set_fraction <- 0.9
test_set_fraction <- 1.0 - training_set_fraction


###### Population Data ######

flog.info("Starting analysis of population census data", name='cl')

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_pop_df$two_class_label <- factor(ifelse(eq3_pop_df$model_class_label == 'allneutral', 'neutral', 'biased'))


# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude <- c("simulation_run_id", "model_class_label", "innovation_rate")
eq3_pop_drop <- eq3_pop_df[,!(names(eq3_pop_df) %in% exclude)]



nonTestIndex <- createDataPartition(eq3_pop_drop$two_class_label, p = training_set_fraction,
                                  list = FALSE,
                                  times = 1)

eq3_pop_nontest <- eq3_pop_drop[ nonTestIndex,]
eq3_pop_test  <- eq3_pop_drop[-nonTestIndex,]

flog.info("Train set size: %s", nrow(eq3_pop_nontest), name='cl')
flog.info("Test set size: %s", nrow(eq3_pop_test), name='cl')

####### 

start_time <- proc.time()[["elapsed"]]
training_fit <- train(two_class_label ~ ., data = eq3_pop_nontest,
                        method="rf",
                        verbose=TRUE,
                        trControl = fit_control,
                        tuneGrid = fit_grid)
end_time <- proc.time()[["elapsed"]]
pop_training_minutes <- (end_time - start_time) / 60

flog.info("Training random forest for population data - elapsed time (min): %s", pop_training_minutes, name='cl')

# now predict the test data using the fit, derive a confusion matrix
# make the class label numeric for pROC
eq3_pop_test$class <- ifelse(eq3_pop_test$two_class_label == 'neutral', 0, 1)
pop_test_predictions <- predict(training_fit, newdata=eq3_pop_test)
pop_test_predictions_numeric <- ifelse(pop_test_predictions == 'neutral', 0, 1)
pop_confusion <- confusionMatrix(pop_test_predictions, eq3_pop_test$two_class_label)
pop_test_roc <- roc(pop_test_predictions_numeric, eq3_pop_test$class)


# variable importance
pop_varimp <- varImp(training_fit)


########################### sampled data ##########################

flog.info("Starting analysis of sampled data set")

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_sampled_df$two_class_label <- factor(ifelse(eq3_sampled_df$model_class_label == 'allneutral', 'neutral', 'biased'))


## sample size 20 ##

eq3_sampled_20 <- filter(eq3_sampled_df, sample_size == 20)

# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude <- c("simulation_run_id", "model_class_label", "innovation_rate", "sample_size")
eq3_sampled_drop <- eq3_sampled_20[,!(names(eq3_sampled_20) %in% exclude)]

nonTestIndex <- createDataPartition(eq3_sampled_drop$two_class_label, p = training_set_fraction,
                                    list = FALSE,
                                    times = 1)

eq3_sampled_nontest_20 <- eq3_sampled_drop[ nonTestIndex,]
eq3_sampled_test_20  <- eq3_sampled_drop[-nonTestIndex,]

flog.info("Train set size: %s", nrow(eq3_sampled_nontest_20), name='cl')
flog.info("Test set size: %s", nrow(eq3_sampled_test_20), name='cl')


####### 



start_time <- proc.time()[["elapsed"]]

sampled_training_fit <- train(two_class_label ~ ., data = eq3_sampled_nontest_20,
                      method="rf",
                      verbose=TRUE,
                      trControl = fit_control,
                      tuneGrid = fit_grid)

end_time <- proc.time()[["elapsed"]]
sampled_training_minutes <- (end_time - start_time) / 60

flog.info("Training random forest for sampled data - elapsed time (min): %s", sampled_training_minutes, name='cl')

# now predict the test data using the fit, derive a confusion matrix
eq3_sampled_test_20$class <- ifelse(eq3_sampled_test_20$two_class_label == 'neutral', 0, 1)
sampled_test_predictions <- predict(sampled_training_fit, newdata=eq3_sampled_test_20)
sampled_test_predictions_numeric <- ifelse(sampled_test_predictions == 'neutral', 0, 1)
sampled_confusion <- confusionMatrix(sampled_test_predictions, eq3_sampled_test_20$two_class_label)
sampled_test_roc <- roc(sampled_test_predictions_numeric, eq3_sampled_test_20$class)

# variable importance
sampled_varimp <- varImp(sampled_training_fit)



########################### TA and sampled data ##########################

flog.info("Starting analysis of TA and sampled data set")

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_ta_sampled_df$two_class_label <- factor(ifelse(eq3_ta_sampled_df$model_class_label == 'allneutral', 'neutral', 'biased'))


## sample size 20 ##

eq3_ta_sampled_20 <- filter(eq3_ta_sampled_df, sample_size == 20)

# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude <- c("simulation_run_id", "model_class_label", "innovation_rate", "sample_size")
eq3_ta_sampled_drop <- eq3_ta_sampled_20[,!(names(eq3_ta_sampled_20) %in% exclude)]

nonTestIndex <- createDataPartition(eq3_ta_sampled_drop$two_class_label, p = training_set_fraction,
                                    list = FALSE,
                                    times = 1)

eq3_ta_sampled_nontest_20 <- eq3_ta_sampled_drop[ nonTestIndex,]
eq3_ta_sampled_test_20  <- eq3_ta_sampled_drop[-nonTestIndex,]

flog.info("Train set size: %s", nrow(eq3_ta_sampled_nontest_20), name='cl')
flog.info("Test set size: %s", nrow(eq3_ta_sampled_test_20), name='cl')


####### 



start_time <- proc.time()[["elapsed"]]

sampled_training_fit <- train(two_class_label ~ ., data = eq3_ta_sampled_nontest_20,
                              method="rf",
                              verbose=TRUE,
                              trControl = fit_control,
                              tuneGrid = fit_grid)

end_time <- proc.time()[["elapsed"]]
sampled_training_minutes <- (end_time - start_time) / 60

flog.info("Training random forest for sampled data - elapsed time (min): %s", sampled_training_minutes, name='cl')

# now predict the test data using the fit, derive a confusion matrix
eq3_ta_sampled_test_20$class <- ifelse(eq3_ta_sampled_test_20$two_class_label == 'neutral', 0, 1)
sampled_test_predictions <- predict(sampled_training_fit, newdata=eq3_ta_sampled_test_20)
sampled_test_predictions_numeric <- ifelse(sampled_test_predictions == 'neutral', 0, 1)
sampled_confusion <- confusionMatrix(sampled_test_predictions, eq3_ta_sampled_test_20$two_class_label)
sampled_test_roc <- roc(sampled_test_predictions_numeric, eq3_ta_sampled_test_20$class)

# variable importance
sampled_varimp <- varImp(sampled_training_fit)




############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "equifinality-3", filename = "classification-caret-results.RData")
flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save.image(image_file)


# End
flog.info("Analysis complete", name='cl')


