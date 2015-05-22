## FILE: perlocus-analysis.r
## EXPERIMENT:  equifinality-5
## PROJECT:  experiment-ctmixtures
## AUTHOR:  Mark E. Madsen
## DATE:  5.20.15

# Purpose:  To take simulated output from the CTMixtures simulation, which is time averaged and sampled,
# and perform gradient boosted classification to measure our ability to distinguish between neutral,
# conformist, and anticonformist cultural transmission given the availability of 26 predictor variables.
# The models are fit using the Caret package to find the optimal model tuning (via 5 repeated of 10-fold CV),
# and the final model assessed using a 20% test hold-out sample.  Test sample results are given by a
# confusion matrix and ROC analysis, which are saved into a set of results objects and saved as binary
# images to the filesystem for later use in paper preparation.
# mmadsenr brings in GBM and other crucial analytic libraries

library(caret)
library(doMC)
library(mmadsenr)
library(futile.logger)
library(dplyr)
library(ggthemes)





log_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", filename = "perlocus-population-classification.log")
flog.appender(appender.file(log_file), name='cl')

clargs <- commandArgs(trailingOnly = TRUE)
if(length(clargs) == 0) {
  pop_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", filename = "equifinality-5-population-data.rda")
} else {
  pop_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", filename = "equifinality-5-population-data.rda", args = clargs)
}

load(pop_data_file)

flog.info("Loaded data file: %s", pop_data_file, name='cl')

flog.info("Beginning classification analysis of equifinality-5 data sets", name='cl')

# set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
num_cores <- get_parallel_cores_given_os(dev=FALSE)
flog.info("Number of cores used in analysis: %s", num_cores, name='cl')
registerDoMC(cores = num_cores)


########### Training and Tuning Variables ##############

#
# Common training and tuning parameters for ctmixtures analysis
#

gbm_grid <- expand.grid(interaction.depth = (1:6)*2,
                        n.trees = (2:10)*50, 
                        shrinkage = 0.05,
                        n.minobsinnode = 10)

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
experiment_names <- c("Per-Locus Census: Neutral vs Conformist","Per-Locus Census: Neutral vs Anticonformist", "Per-Locus Census: Neutral vs All Biased")
perlocus_results_df <- data.frame()

perlocus_pop_results <- NULL
perlocus_pop_model <- NULL
perlocus_pop_cm <- NULL


# prepare data

eq5_neutral_conformist_df <- subset(eq5_pop_df, eq5_pop_df$model_class_label == "neutral" | eq5_pop_df$model_class_label == "conformist")
eq5_neutral_anticonformist_df <- subset(eq5_pop_df, eq5_pop_df$model_class_label == "neutral" | eq5_pop_df$model_class_label == "anticonformist")
eq5_pop_df$two_class_label <- factor(ifelse(eq5_pop_df$model_class_label == 'neutral', 'neutral', 'biased'))

############### Neutral vs Conformist Data #####################

flog.info("Starting analysis of population census data", name='cl')

# first row of results
i <- 1
exp_name <- experiment_names[i]

# remove fields from analysis that aren't predictors, and the detailed label with 3 classes
exclude_columns <- c("simulation_run_id", "innovation_rate", "configuration_slatkin", "num_trait_configurations")


# fit GBM model
model <- train_gbm_classifier(eq5_neutral_conformist_df, training_set_fraction, "model_class_label", gbm_grid, training_control, exclude_columns, verbose=FALSE)


perlocus_pop_model[["perlocus_census_conformist"]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$model_class_label)
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments <- exp_name
results$elapsed <- model$elapsed
perlocus_pop_cm[["perlocus_census_conformist"]] <- cm

# calculate a ROC curve
population_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "model_class_label", exp_name)
results$auc <- unlist(population_roc$auc@y.values)
perlocus_pop_results[["perlocus_census_conformist"]]  <- population_roc

perlocus_results_df <- rbind(perlocus_results_df, results)

################ Neutral vs Anticonformist ##################

# first row of results
i <- 2
exp_name <- experiment_names[i]

# remove fields from analysis that aren't predictors, and the detailed label with 3 classes
exclude_columns <- c("simulation_run_id", "innovation_rate", "configuration_slatkin", "num_trait_configurations")


# fit GBM model
model <- train_gbm_classifier(eq5_neutral_anticonformist_df, training_set_fraction, "model_class_label", gbm_grid, training_control, exclude_columns, verbose=FALSE)


perlocus_pop_model[["perlocus_census_anticonformist"]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$model_class_label)
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments <- exp_name
results$elapsed <- model$elapsed
perlocus_pop_cm[["perlocus_census_anticonformist"]] <- cm

# calculate a ROC curve
population_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "model_class_label", exp_name)
results$auc <- unlist(population_roc$auc@y.values)
perlocus_pop_results[["perlocus_census_anticonformist"]]  <- population_roc

perlocus_results_df <- rbind(perlocus_results_df, results)



################ Neutral vs Both Biased Models ##################

# first row of results
i <- 3
exp_name <- experiment_names[i]

# remove fields from analysis that aren't predictors
exclude_columns <- c("simulation_run_id", "model_class_label", "innovation_rate", "configuration_slatkin", "num_trait_configurations")


# fit GBM model
model <- train_gbm_classifier(eq5_pop_df, training_set_fraction, "two_class_label", gbm_grid, training_control, exclude_columns, verbose=FALSE)


perlocus_pop_model[["perlocus_census_biased"]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$two_class_label)
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments <- exp_name
results$elapsed <- model$elapsed
perlocus_pop_cm[["perlocus_census_biased"]] <- cm

# calculate a ROC curve
population_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", exp_name)
results$auc <- unlist(population_roc$auc@y.values)
perlocus_pop_results[["population_census_biased"]]  <- population_roc

perlocus_results_df <- rbind(perlocus_results_df, results)





############## Complete Processing and Save perlocus_results_df ##########3

# we can now use plot_multiple_roc() to plot all the ROC curves on the same plot, etc.  
# as well as graph various of the metrics as they vary across sample size and TA duratio
#plot_multiple_roc_from_list(popsampled_perlocus_pop_results)

if(length(clargs) == 0) {
  
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", 
                              filename = "classification-perlocus-results-gbm.RData")
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", 
                                      filename = "classification-perlocus-results-gbm-dfOnly.RData")
  
  
} else {
  
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", 
                              filename = "classification-perlocus-results-gbm.RData", args = clargs)
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", 
                                      filename = "classification-perlocus-results-gbm-dfOnly.RData", args = clargs)
}

flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save(perlocus_results_df, perlocus_pop_model, perlocus_pop_results, perlocus_pop_cm, file=image_file)

flog.info("Saving just data frame of results of analysis to R environment snapshot: %s", image_file_results, name='cl')
save(perlocus_results_df, file=image_file_results)

# End
flog.info("Analysis complete", name='cl')


