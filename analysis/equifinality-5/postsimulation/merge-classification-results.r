#
# Combine the results of the two classification runs with gradient boosted models
# 
# The population and sampled classifications are in a data frame called "results"
# The tasampled classifications are in a data frame called "subsets" since they're subsets of the single large database
# The combined (non-subsetted) tasampled classification is in a data frame called "combined_results"
#
library(mmadsenr)


results_files <- c(
"classification-combined-tassize-result-gbm-dfonly.RData",
"classification-population-results-gbm-dfOnly.RData",
"classification-ta-sampled-results-gbm-dfonly.RData",
"per-locus-analysis-gbm-dfonly.RData",
"perlocus-tassize-results-gbm-dfonly.RData"
)




for(file in results_files) {
  load(get_data_path(suffix = "experiment-ctmixtures/equifinality-5/results", filename = file))
}


# Step #1:  Add sample size and ta duration columns to the results data frame
results$sample_size <- 0
results$ta_duration <- 0  # this broadcasts 0 to the entire column


# Add an "experiment group" to each df before merging, to be used in visually distinguishing the classes

results$exp_group <- 'Population Census' 
perlocus_results$exp_group <- 'Population Per-Locus Only'
combined_tassize_results$exp_group <- 'Time Averaged and Sampled Combined Intervals' 
tassize_perlocus_results$exp_group <- 'Time Averaged and Sampled Per-Locus Only'
tassize_subsets_results$exp_group <- 'Time Averaged and Sampled'







# now merge the two into a single dataframe
classifier_results <- rbind(results, 
                            perlocus_results, 
                            combined_tassize_results, 
                            tassize_perlocus_results, 
                            tassize_subsets_results)


###### Add two useful statistics #######


classifier_results$fdr <- 1.0 - classifier_results$ppv
classifier_results$youdensj <- classifier_results$sensitivity + classifier_results$specificity - 1.0



############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5/results", filename = "classification-gbm-merged-dfonly.RData")
save(classifier_results, file=image_file)


