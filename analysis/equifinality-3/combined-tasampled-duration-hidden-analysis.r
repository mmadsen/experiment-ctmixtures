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
# In this analysis, we keep all of the sample sizes and ta duration samples together, to see how accurate we 
# can predict with all the data.  BUT, we treat duration as an unknown parameter, while sample size is visible
# to the analysis (and the archaeologis)


# Set up logging
log_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "combined-tasampled-classification.log")
flog.appender(appender.file(log_file), name='cl')

clargs <- commandArgs(trailingOnly = TRUE)
if(length(clargs) == 0) {
  ta_sampled_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "equifinality-3-ta-sampled-data.rda")
} else {
  ta_sampled_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "equifinality-3-ta-sampled-data.rda", args = clargs)
}

load(ta_sampled_data_file)
flog.info("Loaded data file: %s", ta_sampled_data_file, name='cl')


flog.info("Beginning classification analysis of equifinality-3 data sets for combined tasampled with hidden duration", name='cl')

# set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
num_cores <- get_parallel_cores_given_os(dev=TRUE)
flog.info("Number of cores used in analysis: %s", num_cores, name='cl')
registerDoMC(cores = num_cores)

########### Training and Tuning Variables ##############

#
# Common training and tuning parameters for ctmixtures analysis
#

gbm_grid <- expand.grid(.interaction.depth = (1:6)*2,
                        .n.trees = (1:10)*25, 
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


# set up combined_results data frames
experiment_names <- c("All Sample Sizes and TA Durations")
combined_tassize_results <- data.frame()
combined_tassize_results_roc <- NULL
combined_tassize_results_model <- NULL
combined_tassize_results_cm <- NULL

###### Population Data ######

flog.info("Starting analysis of combined tasampled data", name='cl')

# first row of combined_results
i <- 1
exp_name <- experiment_names[i]


# We want to downsample the full 800K points down to 100K, so that we have the same balance of under/overfitting
# with this analysis as all the other analyses which use 100K points.  We do that using a stratified random 
# sample by model class, to ensure good proportional representation for the classifier
sample_rows <- createDataPartition(eq3_ta_sampled_df$model_class_label, p = 1/8, list = FALSE)
eq3_downsampled_df <- eq3_ta_sampled_df[sample_rows,]


# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_downsampled_df$two_class_label <- factor(ifelse(eq3_downsampled_df$model_class_label == 'allneutral', 'neutral', 'biased'))


# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "model_class_label", "innovation_rate", "ta_duration")

#model <- train_randomforest(eq3_downsampled_df, training_set_fraction, fit_grid, fit_control, exclude_columns)
model <- train_gbm_classifier(eq3_downsampled_df, training_set_fraction, "two_class_label", gbm_grid, training_control, exclude_columns, verbose=FALSE)

combined_tassize_results_model[["combined_tassize"]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$two_class_label)
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments <- exp_name
results$elapsed <- model$elapsed
combined_tassize_results_cm[["combined_tassize"]] <- cm

# All other analyses record these
results$sample_size <- 0
results$ta_duration <- 0

# calculate a ROC curve
combined_tassize_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", "Combined Sample Sizes and Durations")
results$auc <- unlist(combined_tassize_roc$auc@y.values)
combined_tassize_results_roc[["combined_tassize"]] <- combined_tassize_roc

combined_tassize_results <- rbind(combined_tassize_results, results)

############## Complete Processing and Save combined_results ##########3

# save objects from the environment

if(length(clargs) == 0) {
  
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                              filename = "classification-combined-tassize-combined_results-gbm.RData")
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                                      filename = "classification-combined-tassize-result-gbm-dfonly.RData")
  
  
} else {
  
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                              filename = "classification-combined-tassize-combined_results-gbm.RData", args = clargs)
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                                      filename = "classification-combined-tassize-result-gbm-dfonly.RData", args = clargs)
}


flog.info("Saving combined_results of analysis to R environment snapshot: %s", image_file, name='cl')
save(combined_tassize_results, combined_tassize_results_model, combined_tassize_results_cm, combined_tassize_results_roc, file=image_file)

flog.info("Saving just data frame of results of analysis to R environment snapshot: %s", image_file_results, name='cl')
save(combined_tassize_results, file=image_file_results)

# End
flog.info("Analysis complete", name='cl')



plot