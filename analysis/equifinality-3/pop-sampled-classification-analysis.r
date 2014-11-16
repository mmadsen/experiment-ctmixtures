library(randomForest)
library(caret)
library(doMC)
library(mmadsenr)
library(futile.logger)
library(dplyr)
library(ggthemes)

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
num_cores <- get_parallel_cores_given_os(dev=TRUE)
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


# Set up sampling of train and test data sets
training_set_fraction <- 0.8
test_set_fraction <- 1.0 - training_set_fraction


# set up results data frames
experiments <- c("Population Census", "Sample Size 10%", "Sample Size 20%")
results <- data.frame(experiments)

results_roc <- NULL
results_model <- NULL

###### Population Data ######

flog.info("Starting analysis of population census data", name='cl')

# first row of results
i <- 1

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_pop_df$two_class_label <- factor(ifelse(eq3_pop_df$model_class_label == 'allneutral', 'neutral', 'biased'))


# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "model_class_label", "innovation_rate")


####### 

#model <- train_randomforest(df, training_set_fraction, fit_grid, fit_control, exclude_columns)
model <- train_gbm_classifier(eq3_pop_df, training_set_fraction, exclude_columns)

results_model[[i]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$two_class_label)
results$kappa[i] <- cm$overall[["Kappa"]]
results$accuracy[i] <- cm$overall[["Accuracy"]]
results$ppv[i] <- cm$byClass[["Pos Pred Value"]]
results$npv[i] <- cm$byClass[["Neg Pred Value"]]
results$postive_label[i] <- cm$positive
results$sensitivity[i] <- cm$byClass[["Sensitivity"]]
results$specificity[i] <- cm$byClass[["Specificity"]]
results$elapsed <- model$elapsed

# calculate a ROC curve
population_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", "Population Census")
results$auc[i] <- unlist(population_roc$auc@y.values)
results_roc[[i]]  <- population_roc

########################### sampled data ##########################

flog.info("Starting analysis of sampled data set", name='cl')


# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_sampled_df$two_class_label <- factor(ifelse(eq3_sampled_df$model_class_label == 'allneutral', 'neutral', 'biased'))

## sample size 10 ##
# second row of results
i <- 2

eq3_sampled_10 <- filter(eq3_sampled_df, sample_size == 10)

# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "model_class_label", "innovation_rate", "sample_size")


####### 

#model <- train_randomforest(df, training_set_fraction, fit_grid, fit_control, exclude_columns)
model <- train_gbm_classifier(eq3_sampled_10, training_set_fraction, exclude_columns)

results_model[[i]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$two_class_label)
results$kappa[i] <- cm$overall[["Kappa"]]
results$accuracy[i] <- cm$overall[["Accuracy"]]
results$ppv[i] <- cm$byClass[["Pos Pred Value"]]
results$npv[i] <- cm$byClass[["Neg Pred Value"]]
results$postive_label[i] <- cm$positive
results$sensitivity[i] <- cm$byClass[["Sensitivity"]]
results$specificity[i] <- cm$byClass[["Specificity"]]
results$elapsed <- model$elapsed

# calculate a ROC curve
ssize_10_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", "Sample Size: 10%")
results$auc[i] <- unlist(ssize_10_roc$auc@y.values)
results_roc[[i]] <- ssize_10_roc

## sample size 20 ##
# second row of results
i <- 3

eq3_sampled_20 <- filter(eq3_sampled_df, sample_size == 20)

# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "model_class_label", "innovation_rate", "sample_size")


####### 

#model <- train_randomforest(df, training_set_fraction, fit_grid, fit_control, exclude_columns)
model <- train_gbm_classifier(eq3_sampled_20, training_set_fraction, exclude_columns)

results_model[[i]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$two_class_label)
results$kappa[i] <- cm$overall[["Kappa"]]
results$accuracy[i] <- cm$overall[["Accuracy"]]
results$ppv[i] <- cm$byClass[["Pos Pred Value"]]
results$npv[i] <- cm$byClass[["Neg Pred Value"]]
results$postive_label[i] <- cm$positive
results$sensitivity[i] <- cm$byClass[["Sensitivity"]]
results$specificity[i] <- cm$byClass[["Specificity"]]
results$elapsed <- model$elapsed

# calculate a ROC curve
ssize_20_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", "Sample Size: 20%")
results$auc[i] <- unlist(ssize_20_roc$auc@y.values)
results_roc[[i]]  <- ssize_20_roc

# we can now use plot_multiple_roc() to plot all the ROC curves on the same plot, etc.  
# as well as graph various of the metrics as they vary across sample size and TA duratio
plot_multiple_roc_from_list(results_roc)



############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "equifinality-3", filename = "classification-pop-sampled-results-gbm.RData")
flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save.image(image_file)

# End
flog.info("Analysis complete", name='cl')


