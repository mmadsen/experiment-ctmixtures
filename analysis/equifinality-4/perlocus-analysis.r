library(randomForest)
library(caret)
library(doMC)
library(mmadsenr)
library(futile.logger)
library(dplyr)
library(ggthemes)



# ANALYSIS:  Per-locus statistics only, no classification data



# Set up logging
log_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "per-locus-only-classification.log")
flog.appender(appender.file(log_file), name='cl')

clargs <- commandArgs(trailingOnly = TRUE)
if(length(clargs) == 0) {
  pop_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "equifinality-3-population-data.rda")
  sampled_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "equifinality-3-sampled-data.rda")
  ta_sampled_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "equifinality-3-ta-sampled-data.rda")
} else {
  pop_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "equifinality-3-population-data.rda", args = clargs)
  sampled_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "equifinality-3-sampled-data.rda", args = clargs)
  ta_sampled_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "equifinality-3-ta-sampled-data.rda", args = clargs)
}

load(pop_data_file)
load(sampled_data_file)
load(ta_sampled_data_file)
flog.info("Loaded data file: %s", pop_data_file, name='cl')
flog.info("Loaded data file: %s", sampled_data_file, name='cl')
flog.info("Loaded data file: %s", ta_sampled_data_file, name='cl')




flog.info("Beginning classification analysis of equifinality-3 data sets for per-locus only predictors", name='cl')

# set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
# dev = TRUE gets ignored on a Linux server and uses the full set of cores
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
experiment_names <- c("Per-Locus Population Census", "Per-Locus Sampled 10%", "Per-Locus Sampled 20%")
perlocus_results <- data.frame()
perlocus_results_roc <- NULL
perlocus_results_model <- NULL
perlocus_results_cm <- NULL

###### Population Data ######

flog.info("Starting analysis of population data without per-locus values only", name='cl')

# first row of combined_results
i <- 1
exp_name <- experiment_names[i]



# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_pop_df$two_class_label <- factor(ifelse(eq3_pop_df$model_class_label == 'allneutral', 'neutral', 'biased'))


# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "model_class_label", "innovation_rate", "configuration_slatkin", "num_trait_configurations")

#model <- train_randomforest(eq3_pop_df, training_set_fraction, fit_grid, fit_control, exclude_columns)
model <- train_gbm_classifier(eq3_pop_df, training_set_fraction, "two_class_label", gbm_grid, training_control, exclude_columns, verbose=FALSE)

perlocus_results_model[["perlocus_pop"]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$two_class_label)
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments <- exp_name
results$elapsed <- model$elapsed
perlocus_results_cm[["perlocus_pop"]] <- cm

# All other analyses record these
results$sample_size <- 0
results$ta_duration <- 0

# calculate a ROC curve
perlocus_pop_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", exp_name)
results$auc <- unlist(perlocus_pop_roc$auc@y.values)
perlocus_results_roc[["perlocus_pop"]] <- perlocus_pop_roc

perlocus_results <- rbind(perlocus_results, results)



###### Sampled Data 10% ######

flog.info("Starting analysis of sampled data without per-locus values only", name='cl')

# first row of combined_results
i <- 2
exp_name <- experiment_names[i]


eq3_sampled_10 <- dplyr::filter(eq3_sampled_df, sample_size == 10)


# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_sampled_10$two_class_label <- factor(ifelse(eq3_sampled_10$model_class_label == 'allneutral', 'neutral', 'biased'))


# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "model_class_label", "innovation_rate", "configuration_slatkin", "num_trait_configurations", "sample_size")

#model <- train_randomforest(eq3_sampled_10, training_set_fraction, fit_grid, fit_control, exclude_columns)
model <- train_gbm_classifier(eq3_sampled_10, training_set_fraction, "two_class_label", gbm_grid, training_control, exclude_columns, verbose=FALSE)

perlocus_results_model[["perlocus_sampled_10"]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$two_class_label)
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments <- exp_name
results$elapsed <- model$elapsed
perlocus_results_cm[["perlocus_sampled_10"]] <- cm

# All other analyses record these
results$sample_size <- 10
results$ta_duration <- 0

# calculate a ROC curve
perlocus_sampled_10_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", exp_name)
results$auc <- unlist(perlocus_sampled_10_roc$auc@y.values)
perlocus_results_roc[["perlocus_sampled_10"]] <- perlocus_sampled_10_roc

perlocus_results <- rbind(perlocus_results, results)


###### Sampled Data 20% ######

flog.info("Starting analysis of sampled data without per-locus values only", name='cl')

# first row of combined_results
i <- 3
exp_name <- experiment_names[i]


eq3_sampled_20 <- dplyr::filter(eq3_sampled_df, sample_size == 10)


# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_sampled_20$two_class_label <- factor(ifelse(eq3_sampled_20$model_class_label == 'allneutral', 'neutral', 'biased'))


# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude_columns <- c("simulation_run_id", "model_class_label", "innovation_rate", "configuration_slatkin", "num_trait_configurations", "sample_size")

#model <- train_randomforest(eq3_sampled_20, training_set_fraction, fit_grid, fit_control, exclude_columns)
model <- train_gbm_classifier(eq3_sampled_20, training_set_fraction, "two_class_label", gbm_grid, training_control, exclude_columns, verbose=FALSE)

perlocus_results_model[["perlocus_sampled_20"]] <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$two_class_label)
results <- get_parsed_binary_confusion_matrix_stats(cm)
results$experiments <- exp_name
results$elapsed <- model$elapsed
perlocus_results_cm[["perlocus_sampled_20"]] <- cm

# All other analyses record these
results$sample_size <- 20
results$ta_duration <- 0

# calculate a ROC curve
perlocus_sampled_20_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", exp_name)
results$auc <- unlist(perlocus_sampled_20_roc$auc@y.values)
perlocus_results_roc[["perlocus_sampled_20"]] <- perlocus_sampled_10_roc

perlocus_results <- rbind(perlocus_results, results)



############## Complete Processing and Save combined_results ##########3

# save objects from the environment
if(length(clargs) == 0) {
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                              filename = "per-locus-analysis-gbm.RData")
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                                      filename = "per-locus-analysis-gbm-dfonly.RData")
  
  
} else {
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                              filename = "per-locus-analysis-gbm.RData",
                              args = clargs)
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                                      filename = "per-locus-analysis-gbm-dfonly.RData", args = clargs)

}

flog.info("Saving combined_results of analysis to R environment snapshot: %s", image_file, name='cl')
save(perlocus_results, perlocus_results_model, perlocus_results_cm, perlocus_results_roc, file=image_file)

flog.info("Saving just data frame of results of analysis to R environment snapshot: %s", image_file_results, name='cl')
save(perlocus_results, file=image_file_results)  

# End
flog.info("Analysis complete", name='cl')



plot