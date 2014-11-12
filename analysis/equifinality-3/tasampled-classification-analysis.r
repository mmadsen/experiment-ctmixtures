library(randomForest)
library(caret)
library(doMC)
library(mmadsenr)
library(futile.logger)
library(dplyr)

# Train and tune random forest classifiers for the ta sampled data set, which is really 8 levels of TA and
# sample size combinations.
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


get_subset_ssize_tadur <- function(df, ssize, tadur) {
  df_subset <- filter(df, sample_size == ssize, ta_duration == tadur)
  df_subset
}




# Set up logging
log_file <- "ta-sampled-classification.log"
flog.appender(appender.file(log_file), name='cl')

flog.info("Beginning classification analysis of TA sampled equifinality-3 data sets", name='cl')

# set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
num_cores <- get_parallel_cores_given_os(dev=FALSE)
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


# tuning grid of parameters and tuning cross-validation parameters
mtry_seq <- seq(from=2, to=20, by=4)
flog.info("Tuning random forest parameter mtry using vals: %s", mtry_seq, name='cl')
fit_grid <- expand.grid(mtry=mtry_seq)

cv_num <- 10
#cv_repeats <- 10

flog.info("Tuning performed by CV, %s folds ", cv_num, name='cl')

fit_control <- trainControl(method="cv", 
                            number=cv_num, 
                            #repeats=cv_repeats, 
                            allowParallel = TRUE,
                            ## Estimate class probabilities
                            classProbs = TRUE)


# Set up sampling of train and test data sets
training_set_fraction <- 0.8
test_set_fraction <- 1.0 - training_set_fraction



# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_ta_sampled_df$two_class_label <- factor(ifelse(eq3_ta_sampled_df$model_class_label == 'allneutral', 'neutral', 'biased'))



############# Process each combination of TA and sample size ###########

# get grid of the sample size and TA duration combinations, to subset the data set
sample_sizes <- unique(eq3_ta_sampled_df$sample_size)
ta_durations <- unique(eq3_ta_sampled_df$ta_dur)

subsets <- expand.grid(sample_size = sample_sizes, ta_duration = ta_durations)

exclude_columns <- c("simulation_run_id", "innovation_rate", "model_class_label", "sample_size", "ta_duration")

subset_dataframes <- NULL
subset_roc <- NULL
subset_roc_ssize_20 <- NULL
subset_roc_ssize_10 <- NULL

# To create a smaller test dataset:
test_tasampled_indices <- createDataPartition(eq3_ta_sampled_df$two_class_label, p = 0.02, list=FALSE)
test_tasampled_df <- eq3_ta_sampled_df[test_tasampled_indices,]


# switch this back to eq3_ta_sampled_df for production

for( i in 1:nrow(subsets)) {
  df <- get_subset_ssize_tadur(test_tasampled_df, 
                              subsets[i, "sample_size"],
                              subsets[i, "ta_duration"])
  print(sprintf("row %d:  sample size: %d  ta duration: %d numrows: %d", i, subsets[i, "sample_size"], subsets[i, "ta_duration"], nrow(df)))
  subset_dataframes[[i]] <- df
  subsets$dataframe_index[i] <- i
  
  model <- train_randomforest(df, training_set_fraction, fit_grid, fit_control, exclude_columns)
  
  # use the test data split by the train_randomforest function and calculate tuned model predictions
  # and then get the confusion matrix and fitting metrics
  predictions <- predict(model$tunedmodel, newdata=model$test_data)
  cm <- confusionMatrix(predictions, model$test_data$two_class_label)
  subsets$kappa[i] <- cm$overall[["Kappa"]]
  subsets$accuracy[i] <- cm$overall[["Accuracy"]]
  subsets$ppv[i] <- cm$byClass[["Pos Pred Value"]]
  subsets$npv[i] <- cm$byClass[["Neg Pred Value"]]
  subsets$postive_label[i] <- cm$positive
  subsets$sensitivity[i] <- cm$byClass[["Sensitivity"]]
  subsets$specificity[i] <- cm$byClass[["Specificity"]]
  
  # calculate a ROC curve
  # TODO = add a real title with ssize and tadur for each curve, these get used for the legend for stacked ROC curves!!
  label <- paste("ss: ", subsets[i, "sample_size"], " ta: ", subsets[i, "ta_duration"])
  roc <- calculate_roc_binary_classifier(model$tunedmodel, model$test_data, "two_class_label", label)
  subset_roc[[i]] <- roc
  subsets$auc[i] <- unlist(roc$auc@y.values)
  
  if(subsets[i, "sample_size"] == 20) {
    subset_roc_ssize_20[[i]] <- roc
  }
  if(subsets[i, "sample_size"] == 10) {
    subset_roc_ssize_10[[i]] <- roc
  }
  
}

# sigh, now we have to remove NULL objects from lists that are subset of the whole analysis
subset_roc_ssize_20 <- subset_roc_ssize_20[-(which(sapply(subset_roc_ssize_20,is.null),arr.ind=TRUE))]
subset_roc_ssize_10 <- subset_roc_ssize_10[-(which(sapply(subset_roc_ssize_10,is.null),arr.ind=TRUE))]


# we can now use plot_multiple_roc() to plot all the ROC curves on the same plot, etc.  
# as well as graph various of the metrics as they vary across sample size and TA duratio
#plot_multiple_roc_from_list(subset_roc)

############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "equifinality-3", filename = "classification-ta-sampled-results.RData")
flog.info("Saving results of analysis to R environment snapshot: %s", image_file, name='cl')
save.image(image_file)


# End
flog.info("Analysis complete", name='cl')


