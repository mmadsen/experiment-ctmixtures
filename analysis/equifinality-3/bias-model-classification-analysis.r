library(randomForest)
library(caret)
library(doMC)
library(mmadsenr)
library(futile.logger)
library(dplyr)
library(ggthemes)

# Train and tune random forest classifiers for each of  data sets coming out of the experiment
# "equifinality-3", for binary analysis. 
#
# THIS EXPERIMENT AIMS AT DIFFERENTIATING VARIOUS BIASED MODELS FROM ONE ANOTHER


############### Set up Execution Environment #############

# Set up logging
log_file <- "biased-model-classification.log"
flog.appender(appender.file(log_file), name='cl')

clargs <- commandArgs(trailingOnly = TRUE)
if(length(clargs) == 0) {
  pop_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-population-data.rda")
  sampled_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-sampled-data.rda")
  ta_sampled_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-ta-sampled-data.rda")
} else {
  pop_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-population-data.rda", args = clargs)
  sampled_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-sampled-data.rda", args = clargs)
  ta_sampled_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-ta-sampled-data.rda", args = clargs)
}

load(pop_data_file)
load(sampled_data_file)
load(ta_sampled_data_file)
flog.info("Loaded data file: %s", pop_data_file, name='cl')
flog.info("Loaded data file: %s", sampled_data_file, name='cl')
flog.info("Loaded data file: %s", ta_sampled_data_file, name='cl')



flog.info("Beginning classification analysis of biased models from equifinality-3 data sets", name='cl')

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
                        .n.trees = (1:10)*25, 
                        .shrinkage = 0.05)

training_control <- trainControl(method="repeatedcv", 
                                 number=10, repeats=5)




############ Setup Results Variables ###############

experiment_names <- c("Conf/Anticonf Dominance: Census", "Conf/Anticonf Dominance: Sample 20%")

bias_results <- data.frame()
bias_results_roc <- NULL
bias_results_model <- NULL
bias_results_cm <- NULL

###### Population Data:  conformist dominant versus anticonformist dominant ######

flog.info("Starting analysis of mixconfdom vs. mixanticonfdom with pop census data", name='cl')

# Row index for results data frame -- bump this by one for each analysis block
i <- 1
exp_name <- experiment_names[i]

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes

eq3_pop_biased <- filter(eq3_pop_df, model_class_label != 'allneutral')
eq3_pop_biasdom_df <- filter(eq3_pop_biased, model_class_label != 'mixconfequal')
eq3_pop_biasdom_df$model_class_label = factor(eq3_pop_biasdom_df$model_class_label, levels = c("mixconfdom", "mixantidom"))

# Two labels are left:  mixconfdom and mixantidom

# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "innovation_rate")

## Train Model ##

bias_dominance_model <- train_gbm_classifier(eq3_pop_biasdom_df, training_set_fraction, "model_class_label", 
                                             gbm_grid, training_control, exclude_columns, verbose=TRUE)
bias_results_model[["bias_dominance_model"]] <- bias_dominance_model$tunedmodel

## Evaluate Model Performance ##

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(bias_dominance_model$tunedmodel, newdata=bias_dominance_model$test_data)
cm <- confusionMatrix(predictions, bias_dominance_model$test_data$model_class_label)
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments[i] <- exp_name
results$elapsed <- bias_dominance_model$elapsed
bias_results_cm[["bias_dominance_model"]] <- cm

# calculate a ROC curve
bias_dominance_roc <- calculate_roc_binary_classifier(bias_dominance_model$tunedmodel, 
                                                      bias_dominance_model$test_data, 
                                                      "model_class_label", 
                                                      exp_name)
results$auc[i] <- unlist(bias_dominance_roc$auc@y.values)
bias_results_roc[["bias_dominance_model"]] <- bias_dominance_roc

# add to the final data frame
bias_results <- rbind(bias_results, results)


###### Sample Size 20 Data:  conformist dominant versus anticonformist dominant ######

flog.info("Starting analysis of mixconfdom vs. mixanticonfdom with sampled size 20 data", name='cl')

# Row index for results data frame -- bump this by one for each analysis block
i <- 2
exp_name <- experiment_names[i]

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes

eq3_sampled_20 <- filter(eq3_sampled_df, sample_size == 20)
eq3_sampled_bias_20 <- filter(eq3_sampled_20, model_class_label != 'allneutral')
eq3_sampled_biasdom_20_df <- filter(eq3_sampled_bias_20, model_class_label != 'mixconfequal')
eq3_sampled_biasdom_20_df$model_class_label = factor(eq3_sampled_biasdom_20_df$model_class_label, levels = c("mixconfdom", "mixantidom"))

# Two labels are left:  mixconfdom and mixantidom

# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "innovation_rate", "sample_size")

## Train Model ##

bias_dominance_20_model <- train_gbm_classifier(eq3_sampled_biasdom_20_df, training_set_fraction, "model_class_label", 
                                                gbm_grid, training_control, exclude_columns, verbose=TRUE)
bias_results_model[["bias_dominance_20_model"]] <- bias_dominance_20_model$tunedmodel

## Evaluate Model Performance ##

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(bias_dominance_20_model$tunedmodel, newdata=bias_dominance_20_model$test_data)
cm <- confusionMatrix(predictions, bias_dominance_20_model$test_data$model_class_label)
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments <- exp_name
results$elapsed <- bias_dominance_20_model$elapsed
bias_results_cm[["bias_dominance_20_model"]] <- cm

# calculate a ROC curve
bias_dominance_roc_20 <- calculate_roc_binary_classifier(bias_dominance_20_model$tunedmodel, 
                                                      bias_dominance_20_model$test_data, 
                                                      "model_class_label", 
                                                      exp_name)
results$auc <- unlist(bias_dominance_roc_20$auc@y.values)
bias_results_roc[["bias_dominance_20_model"]] <- bias_dominance_roc_20

# add to the final data frame
bias_results <- rbind(bias_results, results)




############## Complete Processing and Save Results ##########


# we can now use plot_multiple_roc() to plot all the ROC curves on the same plot, etc.  
# as well as graph various of the metrics as they vary across sample size and TA duration
plot_multiple_roc_from_list(bias_results_roc)



# save objects from the environment
image_file <- get_data_path(suffix = "equifinality-3", filename = "classification-bias-model-comparisons-gbm.RData", args = clargs)
flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save(bias_results, bias_results_roc, bias_results_model, bias_results_cm, file=image_file)

image_file <- get_data_path(suffix = "equifinality-3", filename = "classification-bias-model-comparisons-gbm-dfonly.RData", args = clargs)
flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save(bias_results, file=image_file)


# End
flog.info("Analysis complete", name='cl')



