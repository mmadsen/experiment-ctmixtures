library(randomForest)
library(caret)
library(doMC)
library(mmadsenr)
library(futile.logger)
library(dplyr)
library(ggthemes)

# Train and tune random forest classifiers for each of  data sets coming out of the experiment
# "equifinality-4", for binary analysis. 
#
# THIS EXPERIMENT LOOKS AT THE COMPARISON BETWEEN A NEUTRAL MODEL AND A BALANCED MIXTURE OF CONFORMIST AND ANTICONFORMISTS

get_tassize_subset_ssize_tadur <- function(df, ssize, tadur) {
  df_tassize_subset <- dplyr::filter(df, sample_size == ssize, ta_duration == tadur)
  df_tassize_subset
}


############### Set up Execution Environment #############


log_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = "biasedmodels-classification.log")
flog.appender(appender.file(log_file), name='cl')

clargs <- commandArgs(trailingOnly = TRUE)
if(length(clargs) == 0) {
  pop_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = "equifinality-3-4-population-data.rda")
  ta_sampled_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = "equifinality-3-4-tasampled-data.rda")
} else {
  pop_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = "equifinality-3-4-population-data.rda", args = clargs)
  ta_sampled_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = "equifinality-3-4-tasampled-data.rda", args = clargs)
}

load(pop_data_file)
load(ta_sampled_data_file)
flog.info("Loaded data file: %s", pop_data_file, name='cl')
flog.info("Loaded data file: %s", ta_sampled_data_file, name='cl')





flog.info("Beginning classification analysis of neutral versus balanced bias models from equifinality-4 data sets", name='cl')

# set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
num_cores <- get_parallel_cores_given_os(dev=TRUE)
flog.info("Number of cores used in analysis: %s", num_cores, name='cl')
registerDoMC(cores = num_cores)




# make this repeatable - comment this out or change it to get a fresh analysis result
seed_value <- 58132133
set.seed(seed_value)
flog.info("RNG seed to replicate this analysis: %s", seed_value, name='cl')

# Set up sampling of train and test data sets
training_set_fraction <- 0.8
test_set_fraction <- 1.0 - training_set_fraction

########### Training and Tuning Variables ##############

#
# Common training and tuning parameters for ctmixtures analysis
#

gbm_grid <- expand.grid(.interaction.depth = (1:6)*2,
                        .n.trees = (2:10)*50, 
                        .shrinkage = 0.05)

training_control <- trainControl(method="repeatedcv", 
                                 number=10, repeats=5)




############ Setup Results Variables ###############

experiment_names <- c("Neutral vs Balanced Biased - Population Census")

bias_results <- data.frame()
bias_results_roc <- NULL
bias_results_model <- NULL
bias_results_cm <- NULL

###### Population Data:  conformist dominant versus anticonformist dominant ######

flog.info("Starting analysis of neutral vs. mixconfequal with pop census data", name='cl')

# Row index for results data frame -- bump this by one for each analysis block
i <- 1
exp_name <- experiment_names[i]

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes

eq4_pop_subset <- subset(eq4_pop_df, eq4_pop_df$model_class_label %in% c("mixconfequal", "allneutral"))
eq4_pop_subset$model_class_label = factor(eq4_pop_subset$model_class_label, levels = c("mixconfequal", "allneutral"))

# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "innovation_rate")

## Train Model ##

balanced_bias_neutral_model <- train_gbm_classifier(eq4_pop_subset, training_set_fraction, "model_class_label", 
                                             gbm_grid, training_control, exclude_columns, verbose=FALSE)
bias_results_model[["balanced_bias_neutral_model"]] <- balanced_bias_neutral_model$tunedmodel

## Evaluate Model Performance ##

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(balanced_bias_neutral_model$tunedmodel, newdata=balanced_bias_neutral_model$test_data)
cm <- confusionMatrix(predictions, balanced_bias_neutral_model$test_data$model_class_label, positive = "allneutral")
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments[i] <- exp_name
results$elapsed <- balanced_bias_neutral_model$elapsed
bias_results_cm[["balanced_bias_neutral_model"]] <- cm

# calculate a ROC curve
bias_dominance_roc <- calculate_roc_binary_classifier(balanced_bias_neutral_model$tunedmodel, 
                                                      balanced_bias_neutral_model$test_data, 
                                                      "model_class_label", 
                                                      exp_name)
results$auc[i] <- unlist(bias_dominance_roc$auc@y.values)
bias_results_roc[["balanced_bias_neutral_model"]] <- bias_dominance_roc

# add to the final data frame
bias_results <- rbind(bias_results, results)

bias_results$sample_size <- 0
bias_results$ta_duration <- 0

############# Process each combination of TA and sample size ###########

eq4_ta_sampled_biased_df <- subset(eq4_ta_sampled_df, eq4_ta_sampled_df$model_class_label %in% c("mixconfequal", "allneutral"))
eq4_ta_sampled_biased_df$model_class_label = factor(eq4_ta_sampled_biased_df$model_class_label, levels = c("mixconfequal", "allneutral"))


# get grid of the sample size and TA duration combinations, to tassize_subset the data set
sample_sizes <- unique(eq4_ta_sampled_biased_df$sample_size)
ta_durations <- unique(eq4_ta_sampled_biased_df$ta_dur)

tassize_subsets <- expand.grid(sample_size = sample_sizes, ta_duration = ta_durations)

exclude_columns <- c("simulation_run_id","innovation_rate", "sample_size", "ta_duration")

experiment_names <- character(nrow(tassize_subsets))

# Add experiment names to the tassize_subsets since I didn't do this in the original analysis
for( i in 1:nrow(tassize_subsets)) {
  experiment_names[i] <- paste("Neutral vs Balanced Biased - Sample Size: ", tassize_subsets[i, "sample_size"], " Duration: ", tassize_subsets[i, "ta_duration"])
}

tassize_biased_results <- data.frame()
tassize_biased_roc <- NULL
tassize_biased_roc_ssize_20 <- NULL
tassize_biased_roc_ssize_10 <- NULL
tassize_biased_model <- NULL
tassize_biased_cm <- NULL

# To create a smaller test dataset:
# test_tasampled_indices <- createDataPartition(eq4_ta_sampled_biased_df$two_class_label, p = 0.05, list=FALSE)
# test_tasampled_df <- eq4_ta_sampled_biased_df[test_tasampled_indices,]
# switch the DF input to get_tassize_subset_ssize_tadur() back to eq4_ta_sampled_biased_df for production

for( i in 1:nrow(tassize_subsets)) {
  exp_name <- experiment_names[i]
  df <- get_tassize_subset_ssize_tadur(eq4_ta_sampled_biased_df, 
                              tassize_subsets[i, "sample_size"],
                              tassize_subsets[i, "ta_duration"])
  print(sprintf("row %d:  sample size: %d  ta duration: %d numrows: %d", i, tassize_subsets[i, "sample_size"], tassize_subsets[i, "ta_duration"], nrow(df)))
  
  #model <- train_randomforest(df, training_set_fraction, fit_grid, fit_control, exclude_columns)
  model <- train_gbm_classifier(df, training_set_fraction, "model_class_label", gbm_grid, training_control, exclude_columns, verbose=FALSE)
  
  tassize_biased_model[[exp_name]] <- model$tunedmodel
  
  # use the test data split by the train_randomforest function and calculate tuned model predictions
  # and then get the confusion matrix and fitting metrics
  predictions <- predict(model$tunedmodel, newdata=model$test_data)
  cm <- confusionMatrix(predictions, model$test_data$model_class_label, positive = "allneutral")
  results <- get_parsed_binary_confusion_matrix_stats(cm)
  results$experiments <- experiment_names[i]
  results$elapsed <- model$elapsed
  results$sample_size <- tassize_subsets[i, "sample_size"]
  results$ta_duration <- tassize_subsets[i, "ta_duration"]
  results$experiments <- exp_name
  tassize_biased_cm[[exp_name]] <- cm
  
  roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "model_class_label", experiment_names[i])
  tassize_biased_roc[[exp_name]] <- roc
  results$auc <- unlist(roc$auc@y.values)
  
  if(tassize_subsets[i, "sample_size"] == 20) {
    tassize_biased_roc_ssize_20[[exp_name]] <- roc
  }
  if(tassize_subsets[i, "sample_size"] == 10) {
    tassize_biased_roc_ssize_10[[exp_name]] <- roc
  }


  tassize_biased_results <- rbind(tassize_biased_results, results)

  
}


############## Complete Processing and Save Results ##########

bias_results <- rbind(bias_results, tassize_biased_results)


# we can now use plot_multiple_roc() to plot all the ROC curves on the same plot, etc.  
# as well as graph various of the metrics as they vary across sample size and TA duration
#plot_multiple_roc_from_list(bias_results_roc)


# save objects from the environment

# save objects from the environment
if(length(clargs) == 0) {
  
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", 
                              filename = "balancedbias-neutral-comparison-gbm.RData")
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", 
                                      filename = "balancedbias-neutral-comparison-gbm-dfonly.RData")
  
  
} else {
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", 
                              filename = "balancedbias-neutral-comparison-gbm.RData", args = clargs)
  
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", 
                                      filename = "balancedbias-neutral-comparison-gbm-dfonly.RData", args = clargs)
}

flog.info("Saving combined_results of analysis to R environment snapshot: %s", image_file, name='cl')
save(bias_results, bias_results_model, bias_results_roc, bias_results_cm, tassize_biased_roc, tassize_biased_model, tassize_biased_cm,  file=image_file)


flog.info("Saving just data frame of results of analysis to R environment snapshot: %s", image_file_results, name='cl')
save(bias_results, file=image_file_results)  


# End
flog.info("Analysis bias comparison complete", name='cl')



