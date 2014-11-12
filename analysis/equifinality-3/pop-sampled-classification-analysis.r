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
log_file <- "pop-sampled-classification.log"
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


load(pop_data_file)
load(sampled_data_file)


flog.info("Loaded data file: %s", pop_data_file, name='cl')
flog.info("Loaded data file: %s", sampled_data_file, name='cl')


# make this repeatable - comment this out or change it to get a fresh analysis result
seed_value <- 23581321
set.seed(seed_value)
flog.info("RNG seed to replicate this analysis: %s", seed_value, name='cl')


# tuning grid of parameters and tuning cross-validation parameters
mtry_seq <- seq(from=2, to=12, by=2)
flog.info("Tuning random forest parameter mtry using vals: %s", mtry_seq, name='cl')
fit_grid <- expand.grid(mtry=mtry_seq)

cv_num <- 10

flog.info("Tuning performed by repeated CV, %s folds with %s repeats", cv_num, cv_repeats, name='cl')

fit_control <- trainControl(method="cv", 
                            number=cv_num,  
                            allowParallel = TRUE,
                            ## Estimate class probabilities
                            classProbs = TRUE)


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
pop_model <- train(two_class_label ~ ., data = eq3_pop_nontest,
                        method="rf",
                        verbose=TRUE,
                        trControl = fit_control,
                        tuneGrid = fit_grid)
end_time <- proc.time()[["elapsed"]]
pop_training_minutes <- (end_time - start_time) / 60

flog.info("Training random forest for population data - elapsed time (min): %s", pop_training_minutes, name='cl')

# now predict the test data using the fit, derive a confusion matrix
# make the class label numeric for pROC
pop_test_predictions <- predict(pop_model, newdata=eq3_pop_test)
pop_confusion <- confusionMatrix(pop_test_predictions, eq3_pop_test$two_class_label)
pop_roc <- calculate_roc_binary_classifier(pop_model$tunedmodel, pop_model$test_data, "two_class_label", "Population Census")

########################### sampled data ##########################

flog.info("Starting analysis of sampled data set")

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_sampled_df$two_class_label <- factor(ifelse(eq3_sampled_df$model_class_label == 'allneutral', 'neutral', 'biased'))



## sample size 10 ##

eq3_sampled_10 <- filter(eq3_sampled_df, sample_size == 10)

# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude <- c("simulation_run_id", "model_class_label", "innovation_rate", "sample_size")
eq3_sampled_drop <- eq3_sampled_10[,!(names(eq3_sampled_10) %in% exclude)]

nonTestIndex <- createDataPartition(eq3_sampled_drop$two_class_label, p = training_set_fraction,
                                    list = FALSE,
                                    times = 1)

eq3_sampled_nontest_10 <- eq3_sampled_drop[ nonTestIndex,]
eq3_sampled_test_10  <- eq3_sampled_drop[-nonTestIndex,]

flog.info("Train set size: %s", nrow(eq3_sampled_nontest_10), name='cl')
flog.info("Test set size: %s", nrow(eq3_sampled_test_10), name='cl')

start_time <- proc.time()[["elapsed"]]

sampled_model_10 <- train(two_class_label ~ ., data = eq3_sampled_nontest_10,
                          method="rf",
                          verbose=TRUE,
                          trControl = fit_control,
                          tuneGrid = fit_grid)

end_time <- proc.time()[["elapsed"]]
sampled_training_minutes_10 <- (end_time - start_time) / 60

flog.info("Training random forest for sampled data ssize 10 - elapsed time (min): %s", sampled_training_minutes_10, name='cl')

# now predict the test data using the fit, derive a confusion matrix
sampled_test_predictions_10 <- predict(sampled_model_10, newdata=eq3_sampled_test_10)
sampled_confusion_10 <- confusionMatrix(sampled_test_predictions_10, eq3_sampled_test_10$two_class_label)
sampled_roc_10 <- calculate_roc_binary_classifier(sampled_model_10$tunedmodel, sampled_model_10$test_data, "two_class_label", "Sample Size 10")




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

start_time <- proc.time()[["elapsed"]]

sampled_model_20 <- train(two_class_label ~ ., data = eq3_sampled_nontest_20,
                      method="rf",
                      verbose=TRUE,
                      trControl = fit_control,
                      tuneGrid = fit_grid)

end_time <- proc.time()[["elapsed"]]
sampled_training_minutes_20 <- (end_time - start_time) / 60

flog.info("Training random forest for sampled data ssize 20 - elapsed time (min): %s", sampled_training_minutes_20, name='cl')

# now predict the test data using the fit, derive a confusion matrix
sampled_test_predictions_20 <- predict(sampled_model_20, newdata=eq3_sampled_test_20)
sampled_confusion_20 <- confusionMatrix(sampled_test_predictions_20, eq3_sampled_test_20$two_class_label)
sampled_roc_20 <- calculate_roc_binary_classifier(sampled_model_20$tunedmodel, sampled_model_20$test_data, "two_class_label", "Sample Size 10")











############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "equifinality-3", filename = "classification-caret-results.RData")
flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save.image(image_file)


# End
flog.info("Analysis complete", name='cl')


