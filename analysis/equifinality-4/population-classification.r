library(randomForest)
library(caret)
library(doMC)
library(mmadsenr)
library(futile.logger)
library(dplyr)
library(ggthemes)



# Train and tune random forest classifiers for each of the three data sets coming out of the experiment
# "equifinality-4", for binary analysis. 
#
# Assumes that data-preparation.r has previously loaded CSV files and created binary 
# data files, stored in a local data directory.  
#
# Reduces the four class data into a two class problem for basic classifier analysis, and it splits off
# a test data set, placing it in the environment.  
# 
# NOTE:  This analysis takes a LONG time to run, so it is kept separate from the Rmarkdown
# analysis script.  At the completion of the analysis, it saves an environment image, which
# other scripts, such as RMarkdown documents, can load, and access the fitted model popsampled_results.  
#
# Only the model training and fitting is done in this script.  The test data set is not analyzed
# here, since it can be done quickly enough that I want to be working in RMarkdown to examine different
# options.  

data_directory <- "/mnt"
log_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = "population-classification.log")
flog.appender(appender.file(log_file), name='cl')

clargs <- commandArgs(trailingOnly = TRUE)
if(length(clargs) == 0) {
  pop_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = "equifinality-3-4-population-data.rda")
} else {
  pop_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = "equifinality-3-4-population-data.rda", args = clargs)
}

load(pop_data_file)

flog.info("Loaded data file: %s", pop_data_file, name='cl')

flog.info("Beginning classification analysis of equifinality-4 data sets", name='cl')

# set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
num_cores <- get_parallel_cores_given_os(dev=TRUE)
flog.info("Number of cores used in analysis: %s", num_cores, name='cl')
registerDoMC(cores = num_cores)


########### Training and Tuning Variables ##############

#
# Common training and tuning parameters for ctmixtures analysis
#

gbm_grid <- expand.grid(.interaction.depth = (1:6)*2,
                        .n.trees = (2:10)*50, 
                        .shrinkage = 0.05)

training_control <- trainControl(method="repeatedcv", 
                                 number=10, repeats=5)




# make this repeatable - comment this out or change it to get a fresh analysis result
seed_value <- 58132133
set.seed(seed_value)
flog.info("RNG seed to replicate this analysis: %s", seed_value, name='cl')


# Set up sampling of train and test data sets
training_set_fraction <- 0.8
test_set_fraction <- 1.0 - training_set_fraction


# set up popsampled_results data frames
experiment_names <- c("Population Census")
popsampled_results <- data.frame()

results_roc <- NULL
results_model <- NULL
results_cm <- NULL

###### Population Data ######

flog.info("Starting analysis of population census data", name='cl')

# first row of results
i <- 1
exp_name <- experiment_names[i]

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq4_pop_df$two_class_label <- factor(ifelse(eq4_pop_df$model_class_label == 'allneutral', 'neutral', 'biased'))


# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "model_class_label", "innovation_rate")


####### 

#model <- train_randomforest(df, training_set_fraction, fit_grid, fit_control, exclude_columns)
model <- train_gbm_classifier(eq4_pop_df, training_set_fraction, "two_class_label", gbm_grid, training_control, exclude_columns, verbose=FALSE)


results_model[["population_census"]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$two_class_label)
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments <- exp_name
results$elapsed <- model$elapsed
results_cm[["population_census"]] <- cm

# calculate a ROC curve
population_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", exp_name)
results$auc <- unlist(population_roc$auc@y.values)
results_roc[["population_census"]]  <- population_roc

results <- rbind(popsampled_results, results)




############## Complete Processing and Save popsampled_results ##########3

# we can now use plot_multiple_roc() to plot all the ROC curves on the same plot, etc.  
# as well as graph various of the metrics as they vary across sample size and TA duratio
#plot_multiple_roc_from_list(popsampled_results_roc)

if(length(clargs) == 0) {
  
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", 
                              filename = "classification-population-results-gbm.RData")
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", 
                                      filename = "classification-population-results-gbm-dfOnly.RData")
  
  
} else {
  
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", 
                              filename = "classification-population-results-gbm.RData", args = clargs)
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", 
                                      filename = "classification-population-results-gbm-dfOnly.RData", args = clargs)
}

flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save(results, results_model, results_roc, results_cm, file=image_file)

flog.info("Saving just data frame of results of analysis to R environment snapshot: %s", image_file_results, name='cl')
save(results, file=image_file_results)

# End
flog.info("Analysis complete", name='cl')


