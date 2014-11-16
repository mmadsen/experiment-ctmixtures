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
log_file <- "combined-tasampled-classification.log"
flog.appender(appender.file(log_file), name='cl')

flog.info("Beginning classification analysis of equifinality-3 data sets for combined tasampled with hidden duration", name='cl')

# set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
num_cores <- get_parallel_cores_given_os(dev=TRUE)
flog.info("Number of cores used in analysis: %s", num_cores, name='cl')
registerDoMC(cores = num_cores)

# load data frame, results in object "eq3_pop_df" in the workspace
# for dev, source the file "dev-Rprofile" first to set up a base data directory (yours may vary!)
#
ta_sampled_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-ta-sampled-data.rda")
load(ta_sampled_data_file)
flog.info("Loaded data file: %s", ta_sampled_data_file, name='cl')


# make this repeatable - comment this out or change it to get a fresh analysis result
seed_value <- 23581321
set.seed(seed_value)
flog.info("RNG seed to replicate this analysis: %s", seed_value, name='cl')


# Set up sampling of train and test data sets
training_set_fraction <- 0.8
test_set_fraction <- 1.0 - training_set_fraction


# set up results data frames
experiments <- c("All Sample Sizes and TA Durations")
combined_results <- data.frame(experiments)

###### Population Data ######

flog.info("Starting analysis of combined tasampled data", name='cl')

# first row of results
i <- 1


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
model <- train_gbm_classifier(eq3_downsampled_df, training_set_fraction, exclude_columns)

combined_tassize_model <- model$tunedmodel

# use the test data split by the train_randomforest function and calculate tuned model predictions
# and then get the confusion matrix and fitting metrics
predictions <- predict(model$tunedmodel, newdata=model$test_data)
cm <- confusionMatrix(predictions, model$test_data$two_class_label)
combined_results$kappa[i] <- cm$overall[["Kappa"]]
combined_results$accuracy[i] <- cm$overall[["Accuracy"]]
combined_results$ppv[i] <- cm$byClass[["Pos Pred Value"]]
combined_results$npv[i] <- cm$byClass[["Neg Pred Value"]]
combined_results$postive_label[i] <- cm$positive
combined_results$sensitivity[i] <- cm$byClass[["Sensitivity"]]
combined_results$specificity[i] <- cm$byClass[["Specificity"]]
combined_results$elapsed <- model$elapsed

# calculate a ROC curve
combined_tassize_roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", "Combined Sample Sizes and Durations")
combined_results$auc[i] <- unlist(combined_tassize_roc$auc@y.values)


# render these comparable to the other analyses data frame structure
combined_results$sample_size <- 0
combined_results$ta_duration <- 0

############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "equifinality-3", filename = "classification-combined-tassize-results-gbm.RData")
flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save.image(image_file)

# End
flog.info("Analysis complete", name='cl')



