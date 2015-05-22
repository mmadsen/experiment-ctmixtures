## FILE: combined-tasampled-duration-hidden.r
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

library(mmadsenr)
library(caret)
library(doMC)
library(futile.logger)
library(dplyr)
# not sure if the following is necessary anymore
library(ggthemes)


# Overall timing
ptm <- proc.time()



################# Training and Tuning Parameters #############
## Caret training and tuning parameter grid
gbm_grid <- expand.grid(interaction.depth = (1:6)*2,
                        n.trees = (2:10)*50,
                        shrinkage = 0.05,
                        n.minobsinnode = 10)
training_control <- trainControl(method="repeatedcv",
                                 number=10, repeats=5)
# make this repeatable - comment this out or change it to get a fresh analysis result
seed_value <- 58132133
set.seed(seed_value)
# Set up sampling of train and test data sets
training_set_fraction <- 0.8
test_set_fraction <- 1.0 - training_set_fraction


##################### Helper Functions ####################

# Subset of the larger data set (usually the training set) with a given sample size and time
# averaging duration
get_tassize_subset_ssize_tadur <- function(df, ssize) {
  df_tassize_subset <- dplyr::filter(df, sample_size == ssize)
  df_tassize_subset
}


get_experiment_names <- function(comparison_prefix, tassize_subsets) {
  n <- nrow(tassize_subsets)
  experiment_names <- character(n)
  # Add experiment names to the tassize_subsets since I didn't do this in the original analysis
  for( i in 1:n) {
    experiment_names[i] <- paste0(comparison_prefix, ": Sample Size: ", tassize_subsets[i, "sample_size"],
                                 " Duration: ", tassize_subsets[i, "ta_duration"])
  }
  experiment_names
}


do_model_fit_and_test <- function(comparison,exp_name, ssize, ta_dur, target_label, subset_df, gbm_grid, training_control, exclude_columns) {
  results <- NULL
  model <- train_gbm_classifier(subset_df, training_set_fraction, target_label, gbm_grid, training_control, exclude_columns, verbose=FALSE)
  # use the test data split by the train_randomforest function and calculate tuned model predictions
  # and then get the confusion matrix and fitting metrics
  test_data <- model$test_data
  predictions <- predict(model$tunedmodel, newdata=test_data)
  cm <- confusionMatrix(predictions, test_data[[target_label]])
  stats <- get_parsed_binary_confusion_matrix_stats(cm)
  roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, target_label, exp_name)
  stats$auc <- unlist(roc$auc@y.values)
  stats$sample_size <- ssize
  stats$ta_duration <- ta_dur
  stats$elapsed <- model$elapsed
  stats$experiments <- exp_name
  stats$exp_group <- comparison
  # add the CM, ROC, and other information to the results list
  results$sample_size <- ssize
  results$ta_duration <- ta_dur
  results$cm <- cm
  results$roc <- roc
  results$model <- model
  results$stats <- stats
  results$elapsed <- model$elapsed
  results
}


####################### Main Program #######################
log_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", filename = "combined-tasampled-classification.log")
flog.appender(appender.file(log_file), name='cl')
flog.info("================ Combined TA and Sampled Classification Analysis =================", name='cl')
flog.info("RNG seed to replicate this analysis: %s", seed_value, name='cl')

# set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
num_cores <- get_parallel_cores_given_os(dev=TRUE)
flog.info("Number of cores used in analysis: %s", num_cores, name='cl')
registerDoMC(cores = num_cores)

# load the input data
clargs <- commandArgs(trailingOnly = TRUE)
if(length(clargs) == 0) {
  pop_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", filename = "equifinality-5-tasampled-data.rda")
} else {
  pop_data_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", filename = "equifinality-5-tasampled-data.rda", args = clargs)
}
load(pop_data_file)
flog.info("Loaded data file %s with %.0f rows",pop_data_file, nrow(eq5_ta_sampled_df),name='cl')


## To Test, uncomment this
test_tasampled_indices <- createDataPartition(eq5_ta_sampled_df$model_class_label, p = 0.005, list=FALSE)
eq5_ta_sampled_df <- eq5_ta_sampled_df[test_tasampled_indices,]
flog.info("Test downsample to %.0f rows", nrow(eq5_ta_sampled_df), name='cl')


# Prepare data subsets for the three comparisons to be performed for each
eq5_neutral_conformist_df <- subset(eq5_ta_sampled_df, eq5_ta_sampled_df$model_class_label == "neutral" | eq5_ta_sampled_df$model_class_label == "conformist")
eq5_neutral_conformist_df$two_class_label <- factor(eq5_neutral_conformist_df$model_class_label, levels=unique(eq5_neutral_conformist_df$model_class_label))
eq5_neutral_anticonformist_df <- subset(eq5_ta_sampled_df, eq5_ta_sampled_df$model_class_label == "neutral" | eq5_ta_sampled_df$model_class_label == "anticonformist")
eq5_neutral_anticonformist_df$two_class_label <- factor(eq5_neutral_anticonformist_df$model_class_label, levels=unique(eq5_neutral_anticonformist_df$model_class_label))
eq5_ta_sampled_df$two_class_label <- factor(ifelse(eq5_ta_sampled_df$model_class_label == 'neutral', 'neutral', 'biased'))

# get grid of the sample size and TA duration combinations, to tassize_subset the data set
sample_sizes <- unique(eq5_ta_sampled_df$sample_size)
ta_durations <- unique(eq5_ta_sampled_df$ta_dur)
tassize_subsets <- expand.grid(sample_size = sample_sizes)
flog.info("tassize_subsets: number of subset combinations to analyze per experimental comparison: %.0f", nrow(tassize_subsets),name='cl')


################# neutral vs. conformism ##############
flog.info("Starting neutral vs. conformist subsets",name='cl')
combined_neutral_conformist_results <- data.frame()
combined_neutral_conformist_roc <- NULL
combined_neutral_conformist_roc_ssize_20 <- NULL
combined_neutral_conformist_roc_ssize_10 <- NULL
combined_neutral_conformist_model <- NULL
combined_neutral_conformist_cm <- NULL

experiment_names <- get_experiment_names("Neutral vs Conformist", tassize_subsets)
comparison <- "Combined Duration Neutral versus Conformist"

for( i in 1:nrow(tassize_subsets)) {
  exclude_columns <- c("simulation_run_id", "innovation_rate", "model_class_label", "sample_size")
  target_label <- "two_class_label"
  exp_name <- experiment_names[i]
  ssize <- tassize_subsets[i, "sample_size"]
  ta_dur <- 0
  subset_df <- get_tassize_subset_ssize_tadur(eq5_neutral_conformist_df,
                                              ssize)
  result <- do_model_fit_and_test(comparison,experiment_names[i], ssize, 0, target_label, subset_df, gbm_grid, training_control, exclude_columns)
  combined_neutral_conformist_cm[[exp_name]] <- result$cm
  combined_neutral_conformist_model[[exp_name]] <- result$model
  combined_neutral_conformist_roc[[exp_name]] <- result$roc
  combined_neutral_conformist_results <- rbind(combined_neutral_conformist_results, result$stats)
  if(ssize == 20) {
    combined_neutral_conformist_roc_ssize_20[[exp_name]] <- result$roc
  }
  if(ssize == 10) {
    combined_neutral_conformist_roc_ssize_10[[exp_name]] <- result$roc
  }
  logrow <- sprintf("row %.0f:  sample size: %2.0f  ta duration: %3.0f numrows: %.0f  elapsed: %.4f", i, ssize, 0, nrow(subset_df), result$elapsed)
  flog.info("%s", logrow, name='cl')
}


################# neutral vs. anticonformism ##############
flog.info("Starting neutral vs. anticonformist subsets",name='cl')

combined_neutral_anticonformist_results <- data.frame()
combined_neutral_anticonformist_roc <- NULL
combined_neutral_anticonformist_roc_ssize_20 <- NULL
combined_neutral_anticonformist_roc_ssize_10 <- NULL
combined_neutral_anticonformist_model <- NULL
combined_neutral_anticonformist_cm <- NULL

experiment_names <- get_experiment_names("Neutral vs Anticonformist", tassize_subsets)
comparison <- "Combined Duration Neutral versus Anticonformist"

for( i in 1:nrow(tassize_subsets)) {
  exclude_columns <- c("simulation_run_id", "innovation_rate", "model_class_label", "sample_size")
  target_label <- "two_class_label"
  exp_name <- experiment_names[i]
  ssize <- tassize_subsets[i, "sample_size"]
  ta_dur <- 0
  subset_df <- get_tassize_subset_ssize_tadur(eq5_neutral_anticonformist_df,
                                              ssize)
  result <- do_model_fit_and_test(comparison,experiment_names[i], ssize, 0, target_label, subset_df, gbm_grid, training_control, exclude_columns)
  combined_neutral_anticonformist_cm[[exp_name]] <- result$cm
  combined_neutral_anticonformist_model[[exp_name]] <- result$model
  combined_neutral_anticonformist_roc[[exp_name]] <- result$roc
  combined_neutral_anticonformist_results <- rbind(combined_neutral_anticonformist_results, result$stats)
  if(ssize == 20) {
    combined_neutral_anticonformist_roc_ssize_20[[exp_name]] <- result$roc
  }
  if(ssize == 10) {
    combined_neutral_anticonformist_roc_ssize_10[[exp_name]] <- result$roc
  }
  logrow <- sprintf("row %.0f:  sample size: %2.0f  ta duration: %3.0f numrows: %.0f  elapsed: %.4f", i, ssize, 0, nrow(subset_df), result$elapsed)
  flog.info("%s", logrow, name='cl')
}


##################### neutral vs. both conformism and anticonformism ##############
flog.info("Starting neutral vs. biased subsets",name='cl')

combined_neutral_biased_results <- data.frame()
combined_neutral_biased_roc <- NULL
combined_neutral_biased_roc_ssize_20 <- NULL
combined_neutral_biased_roc_ssize_10 <- NULL
combined_neutral_biased_model <- NULL
combined_neutral_biased_cm <- NULL

experiment_names <- get_experiment_names("Neutral vs Biased", tassize_subsets)
comparison <- "Combined Duration Neutral versus Combined Biases"

for( i in 1:nrow(tassize_subsets)) {
  exclude_columns <- c("simulation_run_id", "innovation_rate", "model_class_label","sample_size")
  target_label <- "two_class_label"
  exp_name <- experiment_names[i]
  ssize <- tassize_subsets[i, "sample_size"]
  ta_dur <- 0
  subset_df <- get_tassize_subset_ssize_tadur(eq5_ta_sampled_df,
                                              ssize)
  result <- do_model_fit_and_test(comparison,experiment_names[i], ssize, 0, target_label, subset_df, gbm_grid, training_control, exclude_columns)
  combined_neutral_biased_cm[[exp_name]] <- result$cm
  combined_neutral_biased_model[[exp_name]] <- result$model
  combined_neutral_biased_roc[[exp_name]] <- result$roc
  combined_neutral_biased_results <- rbind(combined_neutral_biased_results, result$stats)
  if(ssize == 20) {
    combined_neutral_biased_roc_ssize_20[[exp_name]] <- result$roc
  }
  if(ssize == 10) {
    combined_neutral_biased_roc_ssize_10[[exp_name]] <- result$roc
  }
  logrow <- sprintf("row %.0f:  sample size: %2.0f  ta duration: %3.0f numrows: %.0f  elapsed: %.4f", i, ssize, 0, nrow(subset_df), result$elapsed)
  flog.info("%s", logrow, name='cl')
}


################## Save all results #####################
if(length(clargs) == 0) {
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5",
                              filename = "combined-ta-sampled-results-gbm.RData")
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5",
                                      filename = "combined-ta-sampled-results-gbm-dfonly.RData")
} else {
  image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5",
                              filename = "combined-ta-sampled-results-gbm.RData", args = clargs)
  image_file_results <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5",
                                      filename = "combined-ta-sampled-results-gbm-dfonly.RData", args = clargs)
}
flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save(combined_neutral_conformist_cm,
    combined_neutral_conformist_results,
    combined_neutral_conformist_model,
    combined_neutral_conformist_roc,
    combined_neutral_conformist_roc_ssize_10,
    combined_neutral_conformist_roc_ssize_20, 
    combined_neutral_anticonformist_results,
    combined_neutral_anticonformist_model,
    combined_neutral_anticonformist_roc,
    combined_neutral_anticonformist_roc_ssize_20,
    combined_neutral_anticonformist_roc_ssize_10,
    combined_neutral_anticonformist_cm,
    combined_neutral_biased_results,
    combined_neutral_biased_model,
    combined_neutral_biased_roc,
    combined_neutral_biased_roc_ssize_20,
    combined_neutral_biased_roc_ssize_10,
    combined_neutral_biased_cm,
    file=image_file)
flog.info("Saving just data frame of results of analysis to R environment snapshot: %s", image_file_results, name='cl')
save(combined_neutral_conformist_results,combined_neutral_anticonformist_results,combined_neutral_biased_results, file=image_file_results)
# End
flog.info("Analysis complete", name='cl')
total_time <- proc.time() - ptm
flog.info("Elapsed time in analysis: %.3f", total_time[3],name='cl')


