#
# Combine the results of the two classification runs with gradient boosted models
# 
# The population and sampled classifications are in a data frame called "results"
# The tasampled classifications are in a data frame called "subsets" since they're subsets of the single large database
# The combined (non-subsetted) tasampled classification is in a data frame called "combined_results"
#
library(mmadsenr)

results_files <- c(
  "balancedbias-neutral-comparison-gbm-dfonly.RData",
  "bias-model-comparisons-gbm-dfonly.RData"
  )

for(file in results_files) {
  load(get_data_path(suffix = "experiment-ctmixtures/equifinality-3/results", filename = file))
}


# Step #1:  Add sample size and ta duration columns to the results data frame
popsampled_results$sample_size[1] <- 0
popsampled_results$sample_size[2] <- 10
popsampled_results$sample_size[3] <- 20
popsampled_results$ta_duration[1] <- 0  # this broadcasts 0 to the entire column


# Add an "experiment group" to each df before merging, to be used in visually distinguishing the classes

popsampled_results$exp_group <- '' 
perlocus_results$exp_group <- ''
combined_tassize_results$exp_group <- '' 
tassize_perlocus_results$exp_group <- ''
tassize_subsets_results$exp_group <- ''







# now merge the two into a single dataframe
classifier_results <- rbind(popsampled_results, 
                            perlocus_results, 
                            combined_tassize_results, 
                            tassize_perlocus_results, 
                            tassize_subsets_results)


############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-3/results", filename = "classification-gbm-merged-dfonly.RData")
save(classifier_results, file=image_file)


