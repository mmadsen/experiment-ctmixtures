library(randomForest)
library(caret)
library(doMC)
library(mmadsenr)
library(futile.logger)
library(dplyr)
library(ggthemes)



# Train and tune random forest classifiers for the ta sampled data set, which is really 8 levels of TA and
# sample size combinations.
#

get_tassize_subset_ssize_tadur <- function(df, ssize, tadur) {
  df_tassize_subset <- dplyr::filter(df, sample_size == ssize, ta_duration == tadur)
  df_tassize_subset
}


# Set up logging
log_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "tasampled-classification.log")
flog.appender(appender.file(log_file), name='cl')


clargs <- commandArgs(trailingOnly = TRUE)
if(length(clargs) == 0) {
  ta_sampled_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "equifinality-3-ta-sampled-data.rda")
} else {
  ta_sampled_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", filename = "equifinality-3-ta-sampled-data.rda", args = clargs)
}

load(ta_sampled_data_file)
flog.info("Loaded data file: %s", ta_sampled_data_file, name='cl')



flog.info("Beginning classification analysis of TA sampled equifinality-3 data sets using per-locus predictors only", name='cl')

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


# # tuning grid of parameters and tuning cross-validation parameters
# mtry_seq <- seq(from=2, to=20, by=4)
# flog.info("Tuning random forest parameter mtry using vals: %s", mtry_seq, name='cl')
# fit_grid <- expand.grid(mtry=mtry_seq)
# 
# cv_num <- 10
# #cv_repeats <- 10
# 
# flog.info("Tuning performed by CV, %s folds ", cv_num, name='cl')
# 
# fit_control <- trainControl(method="cv", 
#                             number=cv_num, 
#                             #repeats=cv_repeats, 
#                             allowParallel = TRUE,
#                             ## Estimate class probabilities
#                             classProbs = TRUE)


# Set up sampling of train and test data sets
training_set_fraction <- 0.8
test_set_fraction <- 1.0 - training_set_fraction



# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_ta_sampled_df$two_class_label <- factor(ifelse(eq3_ta_sampled_df$model_class_label == 'allneutral', 'neutral', 'biased'))



############# Process each combination of TA and sample size ###########

# get grid of the sample size and TA duration combinations, to tassize_subset the data set
sample_sizes <- unique(eq3_ta_sampled_df$sample_size)
ta_durations <- unique(eq3_ta_sampled_df$ta_dur)

tassize_subsets <- expand.grid(sample_size = sample_sizes, ta_duration = ta_durations)

exclude_columns <- c("simulation_run_id", "model_class_label", "innovation_rate", "configuration_slatkin", "num_trait_configurations", "sample_size", "ta_duration")

experiment_names <- character(nrow(tassize_subsets))

# Add experiment names to the tassize_subsets since I didn't do this in the original analysis
for( i in 1:nrow(tassize_subsets)) {
  experiment_names[i] <- paste("Per-Locus Sample Size: ", tassize_subsets[i, "sample_size"], " Duration: ", tassize_subsets[i, "ta_duration"])
}

tassize_perlocus_results <- data.frame()
tassize_perlocus_roc <- NULL
tassize_perlocus_roc_ssize_20 <- NULL
tassize_perlocus_roc_ssize_10 <- NULL
tassize_perlocus_model <- NULL
tassize_perlocus_cm <- NULL

# To create a smaller test dataset:
# test_tasampled_indices <- createDataPartition(eq3_ta_sampled_df$two_class_label, p = 0.05, list=FALSE)
# test_tasampled_df <- eq3_ta_sampled_df[test_tasampled_indices,]
# switch the DF input to get_tassize_subset_ssize_tadur() back to eq3_ta_sampled_df for production

for( i in 1:nrow(tassize_subsets)) {
  exp_name <- experiment_names[i]
  df <- get_tassize_subset_ssize_tadur(eq3_ta_sampled_df, 
                              tassize_subsets[i, "sample_size"],
                              tassize_subsets[i, "ta_duration"])
  print(sprintf("row %d:  sample size: %d  ta duration: %d numrows: %d", i, tassize_subsets[i, "sample_size"], tassize_subsets[i, "ta_duration"], nrow(df)))
  
  #model <- train_randomforest(df, training_set_fraction, fit_grid, fit_control, exclude_columns)
  model <- train_gbm_classifier(df, training_set_fraction, "two_class_label", gbm_grid, training_control, exclude_columns, verbose=FALSE)
  
  tassize_perlocus_model[[exp_name]] <- model$tunedmodel
  
  # use the test data split by the train_randomforest function and calculate tuned model predictions
  # and then get the confusion matrix and fitting metrics
  predictions <- predict(model$tunedmodel, newdata=model$test_data)
  cm <- confusionMatrix(predictions, model$test_data$two_class_label)
  results <- get_parsed_binary_confusion_matrix_stats(cm)
  results$experiments <- experiment_names[i]
  results$elapsed <- model$elapsed
  results$sample_size <- tassize_subsets[i, "sample_size"]
  results$ta_duration <- tassize_subsets[i, "ta_duration"]
  results$experiments <- exp_name
  tassize_perlocus_cm[[exp_name]] <- cm
  
  roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", experiment_names[i])
  tassize_perlocus_roc[[exp_name]] <- roc
  results$auc <- unlist(roc$auc@y.values)
  
  if(tassize_subsets[i, "sample_size"] == 20) {
    tassize_perlocus_roc_ssize_20[[exp_name]] <- roc
  }
  if(tassize_subsets[i, "sample_size"] == 10) {
    tassize_perlocus_roc_ssize_10[[exp_name]] <- roc
  }


  tassize_perlocus_results <- rbind(tassize_perlocus_results, results)

  
}

# sigh, now we have to remove NULL objects from lists that are tassize_subset of the whole analysis
tassize_perlocus_roc_ssize_20 <- tassize_perlocus_roc_ssize_20[-(which(sapply(tassize_perlocus_roc_ssize_20,is.null),arr.ind=TRUE))]
tassize_perlocus_roc_ssize_10 <- tassize_perlocus_roc_ssize_10[-(which(sapply(tassize_perlocus_roc_ssize_10,is.null),arr.ind=TRUE))]


# we can now use plot_multiple_roc() to plot all the ROC curves on the same plot, etc.  
# as well as graph various of the metrics as they vary across sample size and TA duratio
#plot_multiple_roc_from_list(tassize_perlocus_roc)

############## Complete Processing and Save Results ##########3

#save objects from the environment

if(length(clargs) == 0) {
  
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                              filename = "perlocus-tassize-results-gbm.RData")
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                                      filename = "perlocus-tassize-results-gbm-dfonly.RData")
  
  
} else {
  
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                              filename = "perlocus-tassize-results-gbm.RData", args = clargs)
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3", 
                                      filename = "perlocus-tassize-results-gbm-dfonly.RData", args = clargs)
}

flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save(tassize_perlocus_results, tassize_perlocus_cm, tassize_perlocus_model, 
     tassize_perlocus_roc, tassize_perlocus_roc_ssize_10, tassize_perlocus_roc_ssize_20, file=image_file)
flog.info("Saving just data frame of results of analysis to R environment snapshot: %s", image_file_results, name='cl')
save(tassize_perlocus_results, file=image_file_results)

# End
flog.info("Analysis complete", name='cl')


