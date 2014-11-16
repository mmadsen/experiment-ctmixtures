#
# Combine the results of the two classification runs with gradient boosted models
# 
# The population and sampled classifications are in a data frame called "results"
# The tasampled classifications are in a data frame called "subsets" since they're subsets of the single large database
#
library(mmadsenr)

# Step #1:  Add sample size and ta duration columns to the results data frame
results$sample_size[1] <- 0
results$sample_size[2] <- 10
results$sample_size[3] <- 20
results$ta_duration[1] <- 0  # this broadcasts 0 to the entire column

# Add experiment names to the subsets since I didn't do this in the original analysis
for( i in 1:nrow(subsets)) {
  label <- paste("Sample Size: ", subsets[i, "sample_size"], " Duration: ", subsets[i, "ta_duration"])
  subsets$experiments[i] <- label
}



# make a copy of subsets, we're going to nuke a column before the merge
assign("subsets_copy", subsets)
subsets_copy$dataframe_index <- NULL

# now merge the two into a single dataframe
classifier_results <- rbind(subsets_copy, results)
# Now combine the ROC objects into a single list
classifier_roc <- c(results_roc, subset_roc)
# Combine the fitted models for later use -- in the order of analysis in each script
classifier_models <- c(results_model, subset_model)

############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "equifinality-3", filename = "classification-gbm-merged-results.RData")
save(classifier_results, classifier_roc, classifier_models, file=image_file)


