#
# Combine the results of the two classification runs with gradient boosted models
# 
# The population and sampled classifications are in a data frame called "results"
# The tasampled classifications are in a data frame called "subsets" since they're subsets of the single large database
# The combined (non-subsetted) tasampled classification is in a data frame called "combined_results"
#
library(mmadsenr)

# load intermediate results from classifications
load(get_data_path(suffix = "equifinality-3", filename = "classification-combined-tassize-result-gbm-dfonly.RData"))
load(get_data_path(suffix = "equifinality-3", filename = "classification-pop-sampled-results-gbm-dfonly.RData"))
load(get_data_path(suffix = "equifinality-3", filename = "classification-ta-sampled-results-gbm-dfonly.RData"))


# Step #1:  Add sample size and ta duration columns to the results data frame
results$sample_size[1] <- 0
results$sample_size[2] <- 10
results$sample_size[3] <- 20
results$ta_duration[1] <- 0  # this broadcasts 0 to the entire column


# make a copy of subsets, we're going to nuke a column before the merge
assign("subsets_copy", subsets)
subsets_copy$dataframe_index <- NULL

# now merge the two into a single dataframe
classifier_results <- rbind(subsets_copy, results)
classifier_results <- rbind(classifier_results, combined_results)
# need to add misclassification_rate since the original two analyses grabbed info from confusionMatrix objects manually
classifier_results$misclassification_rate <- 1.0 - classifier_results$accuracy


# # Now combine the ROC objects into a single list
# classifier_roc <- c(results_roc, subset_roc)
# # Combine the fitted models for later use -- in the order of analysis in each script
# classifier_models <- c(results_model, subset_model)

############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "equifinality-3", filename = "classification-gbm-merged-results.RData")
save(classifier_results, file=image_file)


